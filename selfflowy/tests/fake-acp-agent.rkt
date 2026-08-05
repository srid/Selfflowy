#! /usr/bin/env racket
#lang racket/base

;; A scripted ACP agent, for testing the bridge without an LLM.
;;
;; It speaks just enough of the protocol to be indistinguishable from a real
;; agent as far as selfflowy/web/acp is concerned: line-delimited JSON-RPC on
;; stdio, initialize / session/new / session/set_mode, one turn per
;; session/prompt, session/cancel as a notification, and session/update
;; notifications on the way.
;;
;; Behaviour is keyed on the prompt text, so a test asks for what it needs:
;;
;;   CRASH        exit 1 mid-turn, after the first chunk
;;   SLOW         dawdle between chunks, long enough to cancel
;;   PERMISSION   ask session/request_permission and wait for the answer
;;   MODEL        switch models mid-turn (a config_option_update)
;;   COMMANDS     offer a different command list mid-turn
;;
;; Every turn also writes one line to stderr: the bridge drains that pipe into
;; the server's log, and a bridge that did not would eventually block here.
;;
;; Dumb and deterministic on purpose. This is test infrastructure, not a
;; simulator: no LLM, no state beyond the session id, no concurrency beyond
;; the one turn thread (the main loop keeps reading, which is what makes
;; cancel and permission answers arrive DURING a turn).
;;
;; Self-contained: racket/base + json, no selfflowy collection. Run it as
;; `racket fake-acp-agent.rkt`, or directly — it is committed executable.
;;
;; Nothing runs on require (raco test would otherwise sit here reading stdin
;; forever); the driver is in `main`.

(require json
         racket/string)

(define out-sema (make-semaphore 1))

(define (emit! js)
  (call-with-semaphore
   out-sema
   (λ ()
     (write-json js (current-output-port))
     (newline (current-output-port))
     (flush-output (current-output-port)))))

(define (respond! id result)
  (emit! (hash 'jsonrpc "2.0" 'id id 'result result)))

(define (notify! method params)
  (emit! (hash 'jsonrpc "2.0" 'method method 'params params)))

(define (noise! str)
  (displayln str (current-error-port))
  (flush-output (current-error-port)))

;; ---- state -----------------------------------------------------------------

(define session-id "fake-session-1")

;; Set by a session/cancel notification, cleared when a prompt is accepted —
;; both in the reading loop, so the two stay in the order they arrived.
(define cancelled? (box #f))

;; Which model this session is running. Reported the way a Claude Code adapter
;; reports it: a session CONFIG OPTION, in the session/new result and again in
;; a config_option_update when it moves.
(define model (box "fake-model-1"))

(define (config-options)
  (list (hash 'id "model"
              'name "Model"
              'description "AI model to use"
              'category "model"
              'type "select"
              'currentValue (unbox model)
              'options (list (hash 'value "fake-model-1" 'name "fake-model-1")
                             (hash 'value "fake-model-2" 'name "fake-model-2")))))

;; The slash commands this session offers, in the adapter's shape: `input` is
;; an argument hint, and is null for a command that takes none.
(define commands
  (box (list (hash 'name "fake-init"
                   'description "start something"
                   'input (json-null))
             (hash 'name "fake-review"
                   'description "look it over"
                   'input (hash 'hint "[deep]")))))

;; The permission answer the turn is waiting for, posted by the main loop.
(define permission-ch (make-channel))

;; Ids for the requests THIS side sends (session/request_permission).
(define next-id (box 1000))

(define (take-id!)
  (define id (unbox next-id))
  (set-box! next-id (add1 id))
  id)

;; ---- the turn --------------------------------------------------------------

(define (update! u)
  (notify! "session/update" (hash 'sessionId session-id 'update u)))

(define (chunk! text)
  (update! (hash 'sessionUpdate "agent_message_chunk"
                 'content (hash 'type "text" 'text text))))

(define (commands!)
  (update! (hash 'sessionUpdate "available_commands_update"
                 'availableCommands (unbox commands))))

;; Sleep in slices so a cancel lands promptly. -> #t if it ran to the end.
(define (dawdle seconds)
  (let loop ([left seconds])
    (cond
      [(unbox cancelled?) #f]
      [(<= left 0) #t]
      [else (sleep 0.05) (loop (- left 0.05))])))

(define (ask-permission!)
  (define id (take-id!))
  (emit! (hash 'jsonrpc "2.0" 'id id 'method "session/request_permission"
               'params (hash 'sessionId session-id
                             'toolCall (hash 'toolCallId "call-1" 'title "read Tasks.rkt")
                             'options (list (hash 'optionId "reject"
                                                  'name "Reject"
                                                  'kind "reject_once")
                                            (hash 'optionId "allow"
                                                  'name "Allow"
                                                  'kind "allow_once")))))
  (sync/timeout 30 permission-ch))

;; The whole script, in one thread. Answers the request itself at the end.
;; The cancelled? box is cleared by the ACCEPTING loop, not here: clearing it
;; in this thread races a session/cancel that follows the prompt immediately,
;; and would swallow it.
(define (run-turn! id text)
  (noise! (format "fake-acp-agent: turn for ~s" text))
  (chunk! "hello ")
  (when (string-contains? text "CRASH")
    (flush-output (current-output-port))
    (exit 1))
  (when (string-contains? text "MODEL")
    (set-box! model "fake-model-2")
    (update! (hash 'sessionUpdate "config_option_update"
                   'configOptions (config-options))))
  (when (string-contains? text "COMMANDS")
    (set-box! commands (list (hash 'name "fake-later"
                                   'description "learned along the way"
                                   'input (json-null))))
    (commands!))
  (when (string-contains? text "PERMISSION")
    (define answer (ask-permission!))
    (unless answer
      (noise! "fake-acp-agent: nobody answered the permission request")))
  (when (string-contains? text "SLOW")
    (dawdle 10))
  (cond
    [(unbox cancelled?)
     (respond! id (hash 'stopReason "cancelled"))]
    [else
     (chunk! "world")
     (update! (hash 'sessionUpdate "tool_call"
                    'toolCallId "call-1"
                    'title "read Tasks.rkt"
                    'kind "read"
                    'status "pending"))
     (update! (hash 'sessionUpdate "tool_call_update"
                    'toolCallId "call-1"
                    'status "completed"))
     ;; one kind the bridge is expected to ignore, to prove it does
     (update! (hash 'sessionUpdate "agent_thought_chunk"
                    'content (hash 'type "text" 'text "thinking")))
     (respond! id (hash 'stopReason "end_turn"))]))

;; ---- the loop --------------------------------------------------------------

(define (prompt-text params)
  (define blocks (hash-ref params 'prompt '()))
  (string-join
   (for/list ([b (in-list (if (list? blocks) blocks '()))]
              #:when (and (hash? b) (equal? (hash-ref b 'type #f) "text")))
     (hash-ref b 'text ""))
   ""))

(define (handle! js)
  (define method (hash-ref js 'method #f))
  (define id (hash-ref js 'id #f))
  (define params (hash-ref js 'params (hash)))
  (cond
    ;; the client answering session/request_permission
    [(and id (not method)) (channel-put permission-ch js)]
    [(equal? method "initialize")
     (respond! id (hash 'protocolVersion 1
                        'agentCapabilities (hash 'loadSession #f)
                        'agentInfo (hash 'name "fake-acp-agent" 'version "1")))]
    [(equal? method "session/new")
     (respond! id (hash 'sessionId session-id 'configOptions (config-options)))]
    [(equal? method "session/set_mode")
     (respond! id (hash))
     ;; The commands go out once the session exists — the adapter sends them
     ;; from a timer right after session/new returns, which is somewhere in
     ;; here. Pinned to set_mode so the order a test sees is the same every
     ;; run: the model (read off the session/new result) first, then these,
     ;; and both before the first prompt can be answered.
     (commands!)]
    [(equal? method "session/cancel") (set-box! cancelled? #t)]
    [(equal? method "session/prompt")
     (define text (prompt-text params))
     (set-box! cancelled? #f)
     (thread (λ () (run-turn! id text)))
     (void)]
    [id
     (emit! (hash 'jsonrpc "2.0" 'id id
                  'error (hash 'code -32601 'message (format "method not found: ~a" method))))]
    [else (void)]))

(define (main)
  (let loop ()
    (define line (read-line (current-input-port) 'any))
    (unless (eof-object? line)
      (unless (string=? (string-trim line) "")
        (define js (with-handlers ([exn:fail? (λ (_e) #f)]) (string->jsexpr line)))
        (when (hash? js) (handle! js)))
      (loop)))
  (exit 0))

(module+ main
  (main))
