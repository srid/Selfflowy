#lang racket/base

;; The ACP bridge, against a scripted agent (tests/fake-acp-agent.rkt): real
;; subprocess, real ndjson, no LLM. What is being checked is the FRAMES — the
;; wire format WP5 renders — and the transcript it replays from.
;;
;; Frames are parsed, never string-matched: they are JSON on the wire and the
;; key order is not a contract.

(require rackunit
         json
         racket/async-channel
         racket/list
         racket/string
         selfflowy/ops
         selfflowy/web/acp)

(define fake-agent
  (path->string (collection-file-path "fake-acp-agent.rkt" "selfflowy" "tests")))

;; A file that exists and is not an executable: the other way to get the
;; agent's path wrong.
(define example
  (build-path (simplify-path (build-path (collection-file-path "info.rkt" "selfflowy")
                                         'up 'up))
              "examples" "Example.rkt"))

;; -> (values agent frame-channel log-port). The bridge is stopped on the way
;; out whether the body finished or not.
(define (with-agent proc)
  (define frames (make-async-channel))
  (define log (open-output-string))
  (define ag (make-acp-agent #:command fake-agent
                             #:cwd (find-system-path 'temp-dir)
                             #:broadcast (λ (name data) (async-channel-put frames (cons name data)))
                             #:log-port log))
  (dynamic-wind void (λ () (proc ag frames log)) (λ () (agent-stop! ag))))

;; Next frame as (cons event-name jsexpr), or #f. Generous: a subprocess boot
;; is in here the first time.
(define (next-frame frames [timeout 30])
  (define f (sync/timeout timeout frames))
  (and f (cons (car f) (string->jsexpr (cdr f)))))

;; Every frame up to and including the first one of `type` — the whole turn as
;; one value, so a test can assert the SEQUENCE rather than poll for parts.
(define (frames-through frames type [timeout 30])
  (let loop ([acc '()] [n 0])
    (cond
      [(> n 20) (reverse acc)]
      [else
       (define f (next-frame frames timeout))
       (cond
         [(not f) (reverse acc)]
         [(equal? (hash-ref (cdr f) 'type #f) type) (reverse (cons f acc))]
         [else (loop (cons f acc) (add1 n))])])))

(define (frame-types fs)
  (for/list ([f (in-list fs)]) (hash-ref (cdr f) 'type #f)))

(define (wait-idle ag [seconds 30])
  (define deadline (+ (current-inexact-milliseconds) (* 1000.0 seconds)))
  (let loop ()
    (cond
      [(not (agent-busy? ag)) #t]
      [(>= (current-inexact-milliseconds) deadline) #f]
      [else (sleep 0.02) (loop)])))

(module+ test
  ;; ---- a whole turn --------------------------------------------------------

  (test-case "a turn is user, the agent's text, its tool lines, then done"
    (with-agent
     (λ (ag frames _log)
       (check-false (agent-busy? ag))
       (agent-prompt! ag "hello there")
       (define fs (frames-through frames "done"))
       (check-equal? (map car fs) (make-list (length fs) "chat"))
       (check-equal? (frame-types fs)
                     '("user" "chunk" "chunk" "tool" "tool" "done"))
       (define js (map cdr fs))
       (check-equal? (hash-ref (list-ref js 0) 'text) "hello there")
       (check-equal? (hash-ref (list-ref js 1) 'text) "hello ")
       (check-equal? (hash-ref (list-ref js 2) 'text) "world")
       ;; one line, two frames: the same id, the status moving
       (check-equal? (hash-ref (list-ref js 3) 'id) "call-1")
       (check-equal? (hash-ref (list-ref js 3) 'title) "read Tasks.rkt")
       (check-equal? (hash-ref (list-ref js 3) 'status) "pending")
       (check-equal? (hash-ref (list-ref js 4) 'id) "call-1")
       (check-equal? (hash-ref (list-ref js 4) 'status) "completed")
       (check-equal? (hash-ref (list-ref js 5) 'stopReason) "end_turn")
       ;; and the transcript is that turn, accumulated
       (check-true (wait-idle ag))
       (define t (agent-transcript ag))
       (check-equal? (length t) 1)
       (check-equal? (car t)
                     (hash 'type "turn"
                           'text "hello there"
                           'agent "hello world"
                           'tools (list (hash 'id "call-1"
                                              'title "read Tasks.rkt"
                                              'status "completed"))
                           'status "done"
                           'stopReason "end_turn"
                           'error (json-null))))))

  ;; An unanswered session/request_permission hangs the turn forever. The
  ;; bridge answers it without asking anybody, so this turn simply finishes.
  (test-case "a permission request is answered, and the turn completes"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "read a file PERMISSION please")
       (define fs (frames-through frames "done"))
       (check-equal? (frame-types fs)
                     '("user" "chunk" "chunk" "tool" "tool" "done"))
       (check-equal? (hash-ref (cdr (last fs)) 'stopReason) "end_turn"))))

  ;; ---- one turn at a time --------------------------------------------------

  (test-case "a second prompt mid-turn is a busy failure, and harms nothing"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "SLOW down")
       ;; wait for the turn to be really under way, not just accepted
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "user")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "chunk")
       (define e
         (with-handlers ([exn:fail:op? values])
           (agent-prompt! ag "and another thing")
           #f))
       (check-pred exn:fail:op? e)
       (check-equal? (exn:fail:op-kind e) 'busy)
       (check-true (agent-busy? ag))
       ;; the first turn is still the only one, and still running
       (define t (agent-transcript ag))
       (check-equal? (length t) 1)
       (check-equal? (hash-ref (car t) 'status) "running")
       (check-equal? (hash-ref (car t) 'text) "SLOW down")
       ;; and it ends by itself when told to
       (agent-cancel! ag)
       (check-true (wait-idle ag)))))

  (test-case "cancel ends the turn through its own response"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "SLOW down")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "user")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "chunk")
       (agent-cancel! ag)
       (define done (cdr (next-frame frames)))
       (check-equal? (hash-ref done 'type) "done")
       (check-equal? (hash-ref done 'stopReason) "cancelled")
       (check-true (wait-idle ag))
       (check-equal? (hash-ref (car (agent-transcript ag)) 'stopReason) "cancelled"))))

  ;; ---- the agent dies ------------------------------------------------------

  (test-case "a crash ends the turn with an error, and the next prompt respawns"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "CRASH now")
       (define fs (frames-through frames "error"))
       (check-equal? (frame-types fs) '("user" "chunk" "error"))
       (check-true (string-contains? (hash-ref (cdr (last fs)) 'message) "exited")
                   (format "~a" (cdr (last fs))))
       (check-true (wait-idle ag))
       ;; no respawn loop: nothing started a process nobody asked for
       (check-equal? (length (agent-transcript ag)) 2)
       ;; the next prompt gets a fresh agent, and a fresh session with it
       (agent-prompt! ag "are you back")
       (check-equal? (frame-types (frames-through frames "done"))
                     '("user" "chunk" "chunk" "tool" "tool" "done"))
       (check-true (wait-idle ag))
       (define t (agent-transcript ag))
       (check-equal? (map (λ (e) (hash-ref e 'type)) t)
                     '("turn" "restart" "turn"))
       (check-equal? (hash-ref (list-ref t 0) 'status) "error")
       (check-true (string-contains? (hash-ref (list-ref t 1) 'message) "new session")
                   (format "~a" (list-ref t 1)))
       (check-equal? (hash-ref (list-ref t 2) 'status) "done"))))

  ;; ---- the log sink --------------------------------------------------------

  ;; The adapter logs to stderr, and a pipe nobody drains eventually blocks the
  ;; process writing into it. Draining it must also never be confused with the
  ;; protocol, which is stdout only.
  (test-case "the agent's stderr lands in the log and not in the frames"
    (with-agent
     (λ (ag frames log)
       (agent-prompt! ag "hello there")
       (check-equal? (frame-types (frames-through frames "done"))
                     '("user" "chunk" "chunk" "tool" "tool" "done"))
       (check-true (wait-idle ag))
       (define noise
         (let loop ([n 0])
           (define s (get-output-string log))
           (cond
             [(string-contains? s "fake-acp-agent") s]
             [(> n 100) s]
             [else (sleep 0.05) (loop (add1 n))])))
       (check-true (string-contains? noise "fake-acp-agent: turn for") noise))))

  ;; ---- new chat ------------------------------------------------------------

  (test-case "reset marks the transcript and says so on the wire"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "hello there")
       (frames-through frames "done")
       (check-true (wait-idle ag))
       (agent-reset! ag)
       (define f (cdr (next-frame frames)))
       (check-equal? (hash-ref f 'type) "reset")
       (define t (agent-transcript ag))
       (check-equal? (map (λ (e) (hash-ref e 'type)) t) '("turn" "reset")))))

  ;; ---- a command that is not an agent --------------------------------------

  (test-case "the bridge refuses a command it cannot run"
    (check-equal? (acp-command-problem fake-agent) #f)
    (check-equal? (acp-command-problem "/nonexistent/acp-agent") "does not exist")
    (check-equal? (acp-command-problem (path->string example)) "is not executable")
    (check-exn exn:fail?
               (λ () (make-acp-agent #:command "/nonexistent/acp-agent"
                                     #:cwd (find-system-path 'temp-dir)
                                     #:broadcast void)))))
