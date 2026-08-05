#lang racket/base

;; The ACP bridge, against a scripted agent (tests/fake-acp-agent.rkt): real
;; subprocess, real ndjson, no LLM. What is being checked is the FRAMES — the
;; wire format WP5 renders — and the transcript it replays from.
;;
;; Frames are parsed, never string-matched: they are JSON on the wire and the
;; key order is not a contract.

(require rackunit
         json
         net/http-client
         net/uri-codec
         racket/async-channel
         racket/file
         racket/list
         racket/port
         racket/string
         selfflowy/ops
         selfflowy/web/acp
         selfflowy/web/markdown
         selfflowy/web/serve)

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

;; A command list as pairs, and its key count beside it. A frame's hashes come
;; back from read-json and the bridge's are its own, so the comparison is over
;; content — and the count is how "and nothing else" is said.
(define (command-pairs cs)
  (for/list ([c (in-list cs)])
    (list (hash-ref c 'name #f) (hash-ref c 'description #f) (hash-count c))))

(define (wait-idle ag [seconds 30])
  (define deadline (+ (current-inexact-milliseconds) (* 1000.0 seconds)))
  (let loop ()
    (cond
      [(not (agent-busy? ag)) #t]
      [(>= (current-inexact-milliseconds) deadline) #f]
      [else (sleep 0.02) (loop)])))

;; ---- a server with an agent in it ------------------------------------------

(define outline
  (string-append "#lang selfflowy\n" "Inbox\n" "  Buy milk\n"))

;; Boots the real server with the fake agent wired in: (proc port agent).
(define (with-server proc)
  (define dir (make-temporary-file "sfacp~a" 'directory))
  (define f (build-path dir "Tasks.rkt"))
  (display-to-file outline f #:exists 'truncate)
  (define bound #f)
  (define agent #f)
  (define stop
    (start-server #:port 0
                  #:bind "127.0.0.1"
                  #:files (list f)
                  #:acp-command fake-agent
                  #:on-listen (λ (p) (set! bound p))
                  #:on-agent (λ (a) (set! agent a))))
  (dynamic-wind
   void
   (λ () (proc bound agent))
   (λ ()
     (stop)
     (delete-directory/files dir))))

;; /events never ends, so this keeps the port. Same shape as tests/serve.rkt.
(define (open-events port)
  (define-values (_status _headers in)
    (http-sendrecv "127.0.0.1" "/events" #:port port #:method #"GET"))
  in)

;; Next real event on the stream: -> (cons name data) | #f. Heartbeats are
;; framing, not news.
(define (next-event in #:timeout [timeout 30])
  (define deadline (+ (current-inexact-milliseconds) (* 1000.0 timeout)))
  (let loop ([name #f] [data '()])
    (define left (/ (- deadline (current-inexact-milliseconds)) 1000.0))
    (define line (and (positive? left)
                      (sync/timeout left (read-line-evt in 'linefeed))))
    (cond
      [(or (not line) (eof-object? line)) #f]
      [(string=? line "")
       (if name (cons name (string-join (reverse data) "\n")) (loop #f '()))]
      [(string-prefix? line "event: ") (loop (substring line 7) data)]
      [(string-prefix? line "data: ") (loop name (cons (substring line 6) data))]
      [else (loop name data)])))

;; -> (values status-code body-string). A form post, the way the panel sends
;; one; `fields` is an alist, url-encoded here rather than by hand.
(define (POST port path [fields '()])
  (define-values (status _headers in)
    (http-sendrecv "127.0.0.1" path #:port port #:method #"POST"
                   #:headers (list #"Content-Type: application/x-www-form-urlencoded")
                   #:data (alist->form-urlencoded fields)))
  (define body (port->string in))
  (close-input-port in)
  (values (string->number (cadr (string-split (bytes->string/utf-8 status) " ")))
          body))

(define (GET port path)
  (define-values (status _headers in)
    (http-sendrecv "127.0.0.1" path #:port port #:method #"GET"))
  (define body (port->string in))
  (close-input-port in)
  (values (string->number (cadr (string-split (bytes->string/utf-8 status) " ")))
          body))

;; Frames off a live /events connection, up to and including the first one of
;; `type`. Same idea as frames-through, one layer out: over HTTP.
(define (events-through in type)
  (let loop ([acc '()] [n 0])
    (cond
      [(> n 20) (reverse acc)]
      [else
       (define ev (next-event in))
       (cond
         [(not ev) (reverse acc)]
         [else
          (define js (string->jsexpr (cdr ev)))
          (if (equal? (hash-ref js 'type #f) type)
              (reverse (cons js acc))
              (loop (cons js acc) (add1 n)))])])))

;; ---- the CLI ---------------------------------------------------------------

;; `selfflowy serve` with SELFFLOWY_ACP_AGENT set to `agent` — or removed from
;; the environment entirely when it is #f. -> (values exit-code stderr).
(define (run-serve agent)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env #"SELFFLOWY_ACP_AGENT"
                              (and agent (string->bytes/utf-8 agent)))
  (define-values (sp stdout stdin stderr)
    (parameterize ([current-environment-variables env])
      (subprocess #f #f #f (find-executable-path "racket")
                  "-l" "selfflowy/cli" "--" "serve" (path->string example))))
  (close-output-port stdin)
  (define out (port->string stdout))
  (define err (port->string stderr))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait sp)
  (values (subprocess-status sp) err))

(module+ test
  ;; ---- a whole turn --------------------------------------------------------

  (test-case "a turn is user, the agent's text, its tool lines, then done"
    (with-agent
     (λ (ag frames _log)
       (check-false (agent-busy? ag))
       (agent-prompt! ag "hello there")
       (define fs (frames-through frames "done"))
       (check-equal? (map car fs) (make-list (length fs) "chat"))
       ;; the `model` and `commands` frames are the session announcing itself:
       ;; the subprocess is spawned by this first prompt, so they land inside
       ;; this first turn
       (check-equal? (frame-types fs)
                     '("user" "model" "commands" "chunk" "chunk" "tool" "tool" "done"))
       (define js (map cdr fs))
       (check-equal? (hash-ref (list-ref js 0) 'text) "hello there")
       (check-equal? (hash-ref (list-ref js 1) 'name) "fake-model-1")
       (check-equal? (hash-ref (list-ref js 3) 'text) "hello ")
       (check-equal? (hash-ref (list-ref js 4) 'text) "world")
       ;; one line, two frames: the same id, the status moving
       (check-equal? (hash-ref (list-ref js 5) 'id) "call-1")
       (check-equal? (hash-ref (list-ref js 5) 'title) "read Tasks.rkt")
       (check-equal? (hash-ref (list-ref js 5) 'status) "pending")
       (check-equal? (hash-ref (list-ref js 6) 'id) "call-1")
       (check-equal? (hash-ref (list-ref js 6) 'status) "completed")
       (check-equal? (hash-ref (list-ref js 7) 'stopReason) "end_turn")
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
                     '("user" "model" "commands" "chunk" "chunk" "tool" "tool" "done"))
       (check-equal? (hash-ref (cdr (last fs)) 'stopReason) "end_turn"))))

  ;; ---- which model ---------------------------------------------------------
  ;;
  ;; The agent's word, never the bridge's guess — and it says it two ways: the
  ;; session config option (what was PICKED) and the CLI's own init message
  ;; (what is RUNNING). They part company at a `/model` slash command.

  (test-case "the model arrives with the session, sticks, and follows a switch"
    (with-agent
     (λ (ag frames _log)
       ;; nothing has been asked yet, so nothing is known
       (check-false (agent-model ag))
       (agent-prompt! ag "hello there")
       (define fs (frames-through frames "done"))
       (check-equal? (frame-types fs)
                     '("user" "model" "commands" "chunk" "chunk" "tool" "tool" "done"))
       (check-equal? (hash-ref (cdr (list-ref fs 1)) 'name) "fake-model-1")
       (check-true (wait-idle ag))
       (check-equal? (agent-model ag) "fake-model-1")
       ;; a session that changes model mid-turn says so, in place
       (agent-prompt! ag "MODEL switch please")
       (define fs2 (frames-through frames "done"))
       (check-equal? (frame-types fs2)
                     '("user" "chunk" "model" "chunk" "tool" "tool" "done"))
       (check-equal? (hash-ref (cdr (list-ref fs2 2)) 'name) "fake-model-2")
       (check-true (wait-idle ag))
       (check-equal? (agent-model ag) "fake-model-2")
       ;; and a third turn on the same model is silent about it
       (agent-prompt! ag "still there")
       (check-equal? (frame-types (frames-through frames "done"))
                     '("user" "chunk" "chunk" "tool" "tool" "done")))))

  ;; A `/model` slash command never reaches the adapter as a config change —
  ;; the wrapped CLI handles it, and the config option goes on naming the model
  ;; the session started on. The live model is in the CLI's `system`/`init`
  ;; message, which the adapter forwards only because session/new asked for it.
  ;; Without both halves the header says "Fable" while every turn runs Opus.
  (test-case "a live model switch that never touches the config option lands anyway"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "hello there")
       (define fs (frames-through frames "done"))
       ;; the first init agrees with the config option, so it is a baseline and
       ;; says nothing: one `model` frame for the session, not two
       (check-equal? (frame-types fs)
                     '("user" "model" "commands" "chunk" "chunk" "tool" "tool" "done"))
       (check-true (wait-idle ag))
       (check-equal? (agent-model ag) "fake-model-1")
       ;; the slash command: a fresh init, no config_option_update
       (agent-prompt! ag "SLASH /model please")
       (define fs2 (frames-through frames "done"))
       (check-equal? (frame-types fs2)
                     '("user" "chunk" "model" "chunk" "tool" "tool" "done"))
       ;; labelled from the picker, which is what a header wants
       (check-equal? (hash-ref (cdr (list-ref fs2 2)) 'name) "Fake Model Three")
       (check-true (wait-idle ag))
       (check-equal? (agent-model ag) "Fake Model Three")
       ;; and the next turn, running the same model, is silent about it
       (agent-prompt! ag "still there")
       (check-equal? (frame-types (frames-through frames "done"))
                     '("user" "chunk" "chunk" "tool" "tool" "done"))
       (check-equal? (agent-model ag) "Fake Model Three"))))

  ;; A running model the picker never offered: the raw id is what a header
  ;; gets. Truthful, and the log says so once so the spelling is findable.
  (test-case "a live model the picker does not offer is shown raw, and logged once"
    (with-agent
     (λ (ag frames log)
       (agent-prompt! ag "hello there")
       (frames-through frames "done")
       (check-true (wait-idle ag))
       (agent-prompt! ag "UNLISTED please")
       (define fs (frames-through frames "done"))
       (check-equal? (frame-types fs)
                     '("user" "chunk" "model" "chunk" "tool" "tool" "done"))
       (check-equal? (hash-ref (cdr (list-ref fs 2)) 'name) "claude-fake-9[1m]")
       (check-true (wait-idle ag))
       (check-equal? (agent-model ag) "claude-fake-9[1m]")
       (define lines (get-output-string log))
       (check-true (string-contains? lines "claude-fake-9[1m]") lines)
       ;; every turn re-announces it; the log says it once
       (agent-prompt! ag "still there")
       (frames-through frames "done")
       (check-true (wait-idle ag))
       (define n (length (regexp-match* #rx"does not offer" (get-output-string log))))
       (check-equal? n 1 (get-output-string log)))))

  ;; ---- which slash commands ------------------------------------------------
  ;;
  ;; The agent's list, whole, every time it moves — and only when it moves. A
  ;; command is invoked as ordinary prompt text, so this is the only thing the
  ;; bridge does with one.

  (test-case "the commands arrive with the session, stick, and follow a change"
    (with-agent
     (λ (ag frames _log)
       ;; nothing has been asked yet, so nothing is offered
       (check-equal? (agent-commands ag) '())
       (agent-prompt! ag "hello there")
       (define fs (frames-through frames "done"))
       (check-equal? (frame-types fs)
                     '("user" "model" "commands" "chunk" "chunk" "tool" "tool" "done"))
       ;; name and description, and nothing else: the argument hint the
       ;; adapter sends is not something the panel draws
       (define offered '(("fake-init" "start something" 2)
                         ("fake-review" "look it over" 2)))
       (check-equal? (command-pairs (hash-ref (cdr (list-ref fs 2)) 'commands)) offered)
       (check-true (wait-idle ag))
       (check-equal? (command-pairs (agent-commands ag)) offered)
       ;; a session that learns a new set says so, in place
       (agent-prompt! ag "COMMANDS please")
       (define fs2 (frames-through frames "done"))
       (check-equal? (frame-types fs2)
                     '("user" "chunk" "commands" "chunk" "tool" "tool" "done"))
       (define later '(("fake-later" "learned along the way" 2)))
       (check-equal? (command-pairs (hash-ref (cdr (list-ref fs2 2)) 'commands)) later)
       (check-true (wait-idle ag))
       (check-equal? (command-pairs (agent-commands ag)) later)
       ;; and a third turn offering the same set is silent about it
       (agent-prompt! ag "still there")
       (check-equal? (frame-types (frames-through frames "done"))
                     '("user" "chunk" "chunk" "tool" "tool" "done")))))

  ;; ---- one turn at a time --------------------------------------------------

  (test-case "a second prompt mid-turn is a busy failure, and harms nothing"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "SLOW down")
       ;; wait for the turn to be really under way, not just accepted
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "user")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "model")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "commands")
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

  ;; The stop button is reachable the instant the prompt is accepted, and the
  ;; agent does not exist yet at that instant — spawning it and shaking hands
  ;; takes seconds. A cancel in that window used to be a silent no-op and the
  ;; turn ran to completion.
  (test-case "a cancel before the agent has booted still ends the turn"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "SLOW down")
       ;; no session, no subprocess, no chunk: nothing but the echoed prompt
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "user")
       (agent-cancel! ag)
       (define fs (frames-through frames "done"))
       (check-equal? (hash-ref (cdr (last fs)) 'stopReason) "cancelled")
       (check-true (wait-idle ag))
       (define t (agent-transcript ag))
       (check-equal? (length t) 1)
       (check-equal? (hash-ref (car t) 'stopReason) "cancelled"))))

  (test-case "cancel ends the turn through its own response"
    (with-agent
     (λ (ag frames _log)
       (agent-prompt! ag "SLOW down")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "user")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "model")
       (check-equal? (hash-ref (cdr (next-frame frames)) 'type) "commands")
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
       (check-equal? (frame-types fs) '("user" "model" "commands" "chunk" "error"))
       (check-true (string-contains? (hash-ref (cdr (last fs)) 'message) "exited")
                   (format "~a" (cdr (last fs))))
       (check-true (wait-idle ag))
       ;; no respawn loop: nothing started a process nobody asked for
       (check-equal? (length (agent-transcript ag)) 2)
       ;; the next prompt gets a fresh agent, and a fresh session with it —
       ;; running the same model, so nothing is said about it a second time
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
                     '("user" "model" "commands" "chunk" "chunk" "tool" "tool" "done"))
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
                                     #:broadcast void))))

  ;; ---- wired into the server ------------------------------------------------

  ;; The acceptance test for the package: a browser sitting on /events sees the
  ;; agent talk, through the same hub the outline events use.
  (test-case "chat frames reach a real /events connection"
    (with-server
     (λ (port agent)
       (check-pred acp-agent? agent)
       (define in (open-events port))
       (agent-prompt! agent "hello there")
       (define first-ev (next-event in))
       (check-not-false first-ev "no chat event within the timeout")
       (check-equal? (car first-ev) "chat")
       (define js (string->jsexpr (cdr first-ev)))
       (check-equal? (hash-ref js 'type) "user")
       (check-equal? (hash-ref js 'text) "hello there")
       ;; the rest of the turn arrives on the same stream
       (let loop ([n 0])
         (define ev (next-event in))
         (check-not-false ev "the turn did not finish on the stream")
         (define j (string->jsexpr (cdr ev)))
         (unless (or (equal? (hash-ref j 'type) "done") (> n 20))
           (loop (add1 n))))
       (close-input-port in))))

  ;; ---- the chat routes -----------------------------------------------------
  ;;
  ;; The panel POSTs and gets a STATUS; what it draws comes back over the
  ;; stream. So every one of these asserts the frames, not the reply body.

  (test-case "POST /chat is accepted, and the whole turn arrives on /events"
    (with-server
     (λ (port agent)
       (define in (open-events port))
       (define-values (code body) (POST port "/chat" '((text . "hello there"))))
       (check-equal? code 204 body)
       ;; 204 means 204: nothing to render, because the stream renders it
       (check-equal? body "" body)
       (define fs (events-through in "done"))
       (check-equal? (for/list ([f (in-list fs)]) (hash-ref f 'type))
                     '("user" "model" "commands" "chunk" "chunk" "tool" "tool" "done"))
       (check-equal? (hash-ref (car fs) 'text) "hello there")
       (define done (last fs))
       (check-equal? (hash-ref done 'stopReason) "end_turn")
       ;; the done frame carries the finished text as Markdown, and it is the
       ;; markdown module's own output — not a second renderer
       (check-equal? (hash-ref done 'html) (note->html-string "hello world"))
       (close-input-port in))))

  (test-case "an empty message is a 400, and the agent never hears about it"
    (with-server
     (λ (port agent)
       (define-values (code body) (POST port "/chat" '((text . "   "))))
       (check-equal? code 400 body)
       (check-true (string-contains? body "message") body)
       (define-values (code2 body2) (POST port "/chat" '()))
       (check-equal? code2 400 body2)
       (check-equal? (agent-transcript agent) '()))))

  (test-case "a message during a turn is a 409; cancel ends the turn"
    (with-server
     (λ (port agent)
       (define in (open-events port))
       (define-values (code _b) (POST port "/chat" '((text . "SLOW one"))))
       (check-equal? code 204)
       ;; Under way, not merely accepted: the `user` frame goes out before the
       ;; subprocess even exists. The first chunk is the agent actually
       ;; talking, which is the state this 409 is about.
       (check-equal? (for/list ([f (in-list (events-through in "chunk"))])
                       (hash-ref f 'type))
                     '("user" "model" "commands" "chunk"))
       (define-values (busy body) (POST port "/chat" '((text . "and another"))))
       (check-equal? busy 409 body)
       (check-true (string-contains? body "busy") body)
       (define-values (cancelled cbody) (POST port "/chat/cancel"))
       (check-equal? cancelled 204 cbody)
       (define done (last (events-through in "done")))
       (check-equal? (hash-ref done 'stopReason) "cancelled")
       (check-true (wait-idle agent))
       (close-input-port in))))

  (test-case "POST /chat/new pushes a reset, which is what clears the panels"
    (with-server
     (λ (port agent)
       (define in (open-events port))
       (define-values (_c _b) (POST port "/chat" '((text . "hello there"))))
       (events-through in "done")
       (check-true (wait-idle agent))
       (define-values (code body) (POST port "/chat/new"))
       (check-equal? code 204 body)
       (define ev (next-event in))
       (check-not-false ev "no reset frame within the timeout")
       (check-equal? (hash-ref (string->jsexpr (cdr ev)) 'type) "reset")
       (close-input-port in))))

  ;; A browser that reloads (or connects late) missed the frames. The page is
  ;; where the conversation comes back, and everything the agent or the user
  ;; wrote is TEXT in it — an xexpr is what escapes it.
  (test-case "the page replays the transcript, escaped"
    (with-server
     (λ (port agent)
       (define in (open-events port))
       (define-values (_c _b)
         (POST port "/chat" '((text . "run <script>alert(1)</script> please"))))
       (events-through in "done")
       (check-true (wait-idle agent))
       (close-input-port in)
       (define-values (code body) (GET port "/"))
       (check-equal? code 200)
       (check-true (string-contains? body "&lt;script&gt;alert(1)&lt;/script&gt;") body)
       (check-false (string-contains? body "<script>alert(1)") body)
       ;; the agent's finished text is Markdown; the tool line is one line
       (check-true (string-contains? body "<p>hello world</p>") body)
       (check-true (string-contains? body "data-tool-id=\"call-1\"") body)
       (check-true (string-contains? body "data-status=\"completed\"") body)
       ;; the header is replayed too: the model frame was ephemeral, the model
       ;; the bridge learned from it is not
       (check-true (string-contains? body "id=\"sf-chat-model\">fake-model-1<") body)
       ;; and so are the commands, so a reloaded panel completes without
       ;; waiting for the agent to say anything again
       (check-true (string-contains? body "fake-init") body)
       (check-true (string-contains? body "fake-review") body))))

  (test-case "the page carries the panel, its script, and ONE sse connection"
    (with-server
     (λ (port agent)
       (define-values (code body) (GET port "/"))
       (check-equal? code 200)
       (check-true (string-contains? body "id=\"sf-chat\"") body)
       (check-true (string-contains? body "src=\"/static/chat.js\"") body)
       (check-true (string-contains? body "action=\"/chat\"") body)
       (check-true (string-contains? body "data-post=\"/chat/new\"") body)
       (check-true (string-contains? body "data-post=\"/chat/cancel\"") body)
       ;; nothing has booted the agent, so there is nothing to complete yet
       (check-true (string-contains? body "data-commands=\"[]\"") body)
       ;; the panel borrows the page's connection: it subscribes to the chat
       ;; event on the body's stream, and opens nothing of its own
       (check-true (string-contains? body "sse-swap=\"chat\"") body)
       (check-equal? (length (regexp-match* #rx"sse-connect=" body)) 1 body)
       ;; and the script it does that with is a file, not an inline blob
       (define-values (jcode js) (GET port "/static/chat.js"))
       (check-equal? jcode 200)
       (check-false (string-contains? js "new EventSource") js))))

  ;; ---- the CLI contract ----------------------------------------------------

  (test-case "serve with no SELFFLOWY_ACP_AGENT is a usage error naming it"
    (define-values (code err) (run-serve #f))
    (check-equal? code 1 err)
    (check-true (string-contains? err "SELFFLOWY_ACP_AGENT") err)
    (check-true (string-contains? err "not set") err))

  (test-case "serve with a SELFFLOWY_ACP_AGENT that is not there says which"
    (define-values (code err) (run-serve "/nonexistent/acp-agent"))
    (check-equal? code 1 err)
    (check-true (string-contains? err "SELFFLOWY_ACP_AGENT") err)
    (check-true (string-contains? err "/nonexistent/acp-agent") err))

  (test-case "serve with a SELFFLOWY_ACP_AGENT that is not executable says so"
    (define-values (code err) (run-serve (path->string example)))
    (check-equal? code 1 err)
    (check-true (string-contains? err "is not executable") err)))
