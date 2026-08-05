#lang racket/base

;; The ACP bridge: one subprocess, one conversation.
;;
;; `selfflowy serve` spawns an agent that speaks the Agent Client Protocol
;; (JSON-RPC 2.0, one object per line, over stdio). Everything about that
;; protocol stops here. What the rest of the server sees is a thing it can
;; prompt, cancel, reset and stop, plus a stream of CHAT FRAMES it fans out to
;; browsers — no other module spells "session/prompt".
;;
;; What this owns, and why:
;;
;;   * the subprocess, spawned LAZILY. A server that boots an agent nobody
;;     talked to pays for a node process (and its credential check) on every
;;     restart; the first prompt is what starts one.
;;   * one turn at a time. ACP would queue; a chat panel does not want a queue,
;;     and a second prompt while the agent is talking is a conflict the caller
;;     has to see — exn:fail:op with kind 'busy, the same vocabulary the write
;;     ops use (the CLI maps kinds to exit codes, a route maps them to statuses).
;;   * the answer to session/request_permission. An unanswered one hangs the
;;     turn forever, so it is answered immediately with the first allow-flavored
;;     option. The session asks for bypassPermissions mode at boot; this is the
;;     backstop for when that was refused (it is, running as root).
;;   * the transcript. Frames are ephemeral — a browser that connects late
;;     missed them — so a turn is also accumulated here, and that is what a
;;     page load replays.
;;
;; Threads: the caller's (prompt / cancel / reset / stop), one READER draining
;; the agent's stdout, one draining its stderr into the server's log, and one
;; per TURN waiting for the session/prompt response. Every state change takes
;; `sema`, and a frame is broadcast while it is held, so the order a browser
;; sees is the order the transcript records.
;;
;; Chat frames and transcript entries are append-only, same discipline as
;; json/reply: new keys may appear, existing ones keep their meaning and type.

(require json
         racket/async-channel
         racket/contract
         racket/path
         racket/string
         (only-in selfflowy/fail user-fail)
         (only-in selfflowy/ops exn:fail:op)
         ;; render-time Markdown has one owner; this module only asks it for
         ;; the finished turn's HTML
         (only-in selfflowy/web/markdown note->html-string))

;; The surface the server sees. `make-acp-agent` is told how to reach the
;; outside world (a command, a directory, somewhere to put frames) and nothing
;; else; every other export is a verb or a read.
(provide (contract-out
          [acp-event-name string?]
          [acp-command-problem (-> (or/c path? string?) (or/c string? #f))]
          [make-acp-agent (->* (#:command (or/c path? string?)
                                #:cwd (or/c path? string?)
                                #:broadcast (-> string? string? any))
                               (#:log-port output-port?)
                               acp-agent?)]
          [acp-agent? (-> any/c boolean?)]
          [agent-prompt! (-> acp-agent? string? void?)]
          [agent-cancel! (-> acp-agent? void?)]
          [agent-reset! (-> acp-agent? void?)]
          [agent-stop! (-> acp-agent? void?)]
          [agent-busy? (-> acp-agent? boolean?)]
          [agent-model (-> acp-agent? (or/c string? #f))]
          [agent-transcript (-> acp-agent? (listof hash?))]))

;; The SSE event name chat frames ride under. One owner: the page that
;; subscribes and the bridge that broadcasts agree by requiring it.
(define acp-event-name "chat")

;; The client half of the handshake. fs is false in both directions: this
;; bridge is not an editor and will not serve file reads over the protocol —
;; the agent has the outline on disk and a shell.
(define client-capabilities
  (hash 'fs (hash 'readTextFile #f 'writeTextFile #f)))

(define protocol-version 1)

;; Permissions are a session MODE, asked for once per session. A refusal is
;; not fatal: request_permission is answered anyway.
(define bypass-mode "bypassPermissions")

;; Boot is three small round trips against a process that just started; a turn
;; is a person waiting on an LLM. Only the first gets a deadline.
(define boot-timeout-seconds 30)

;; How long a clean shutdown waits before it stops asking.
(define stop-timeout-seconds 2)

;; A wedged turn must not make "new chat" hang: cancel, wait this long, then
;; take the subprocess away from it.
(define reset-timeout-seconds 5)

;; ---- what a transcript remembers -------------------------------------------

;; One turn: what was asked, the agent text as it accumulated, the tool lines
;; with their latest status (newest first until serialized), and how it ended.
;; status: 'running | 'done | 'error.
;;
;; `sent?` is whether session/prompt is on the wire yet. A turn is accepted
;; long before that (the subprocess may not even exist), and a cancel that
;; arrives in between has nothing the agent could match it against.
(struct turn (text [agent #:mutable] [tools #:mutable]
                   [status #:mutable] [stop #:mutable] [err #:mutable]
                   [sent? #:mutable]))

(struct tool (id [title #:mutable] [status #:mutable]))

;; Not a turn: the conversation itself moved (a reset, or a dead agent whose
;; successor starts with no memory of any of this).
(struct marker (type message))

;; ---- the agent -------------------------------------------------------------

;; command/cwd/broadcast/log-port are the outside world. Everything mutable
;; below is guarded by `sema`, except the spawn/session handshake, which takes
;; `boot-sema` (never both at once, and never `sema` around a blocking call).
;; out-sema serializes writes to the agent's stdin: three threads write there.
;;
;; `cust` is what the subprocess and the bridge's threads are created under.
;; It matters because a prompt arrives on an HTTP request's thread, and the
;; web server gives every connection a custodian it shuts down as soon as the
;; response is written — which would take the turn thread, the reader, and
;; the agent process with it the moment the 204 went out. The bridge's own
;; custodian lives as long as the bridge, not as long as the request that
;; woke it (and is itself a child of the server's, so stopping still stops).
(struct acp-agent (command cwd broadcast log-port cust
                   sema boot-sema out-sema
                   [sp #:mutable] [stdin #:mutable] [stdout #:mutable]
                   [session #:mutable] [model #:mutable]
                   [next-id #:mutable] [pending #:mutable]
                   [busy? #:mutable] [live-turn #:mutable]
                   [cancel-pending? #:mutable]
                   [entries #:mutable]           ; reversed
                   [spawned? #:mutable] [stopped? #:mutable]
                   [seen-kinds #:mutable]))

;; -> #f, or the phrase that says what is wrong with this path as an agent.
;; Both the CLI (which names the environment variable in front of it) and the
;; constructor ask; "executable" is spelled once.
(define (acp-command-problem cmd)
  (define p (if (path? cmd) cmd (string->path cmd)))
  (cond
    [(not (file-exists? p)) "does not exist"]
    [(not (memq 'execute (file-or-directory-permissions p))) "is not executable"]
    [else #f]))

(define (make-acp-agent #:command command
                        #:cwd cwd
                        #:broadcast broadcast
                        #:log-port [log-port (current-error-port)])
  (define problem (acp-command-problem command))
  (when problem
    (user-fail "acp agent ~a: ~a" problem command))
  (acp-agent (simple-form-path (if (path? command) command (string->path command)))
             (simple-form-path (if (path? cwd) cwd (string->path cwd)))
             broadcast log-port
             (make-custodian)
             (make-semaphore 1) (make-semaphore 1) (make-semaphore 1)
             #f #f #f
             #f #f
             0 (hash)
             #f #f
             #f
             '()
             #f #f
             (hash)))

(define (with-state ag proc)
  (call-with-semaphore (acp-agent-sema ag) proc))

;; Anything that must outlive the call that started it — the subprocess, its
;; pipes, the reader threads, a turn — is created in here.
(define (in-custodian ag thunk)
  (parameterize ([current-custodian (acp-agent-cust ag)]) (thunk)))

;; ---- the log ---------------------------------------------------------------

;; The agent's stderr is a log sink, not a channel: the adapter redirects all
;; its console output there, and a pipe nobody drains eventually blocks the
;; process that is writing to it.
(define (log-line ag str)
  (with-handlers ([exn:fail? void])
    (displayln (string-append "acp: " str) (acp-agent-log-port ag))
    (flush-output (acp-agent-log-port ag))))

(define (elide str [n 240])
  (if (> (string-length str) n)
      (string-append (substring str 0 n) "…")
      str))

;; ---- frames ----------------------------------------------------------------
;;
;; A frame is one line of JSON under `acp-event-name`. Callers hold `sema`:
;; broadcasting inside the lock is what keeps the stream and the transcript
;; telling the same story in the same order.
;;
;; The vocabulary, append-only:
;;
;;   {"type":"user","text"}                   the prompt, echoed to every tab
;;   {"type":"chunk","text"}                  agent text, as it arrives
;;   {"type":"tool","id","title","status"}    same id = the same line, updated
;;   {"type":"done","stopReason","html"}      the turn ended; `html` is the
;;                                            turn's agent text rendered as
;;                                            Markdown (web/markdown), which
;;                                            is what a panel swaps in for the
;;                                            plain text the chunks built
;;   {"type":"error","message"}               the turn ended badly
;;   {"type":"reset"}                         new chat: panels clear
;;   {"type":"model","name"}                  which model the session runs,
;;                                            the moment the agent says so —
;;                                            with the session, and again if
;;                                            it changes under one

(define (broadcast! ag js)
  (with-handlers ([exn:fail? (λ (e) (log-line ag (format "broadcast failed: ~a" (exn-message e))))])
    ((acp-agent-broadcast ag) acp-event-name (jsexpr->string js))))

(define (push-entry! ag e)
  (set-acp-agent-entries! ag (cons e (acp-agent-entries ag))))

;; ---- transcript ------------------------------------------------------------

(define (tool->jsexpr t)
  (hash 'id (tool-id t) 'title (tool-title t) 'status (tool-status t)))

(define (turn->jsexpr tn)
  (hash 'type "turn"
        'text (turn-text tn)
        'agent (turn-agent tn)
        'tools (map tool->jsexpr (reverse (turn-tools tn)))
        'status (symbol->string (turn-status tn))
        'stopReason (or (turn-stop tn) (json-null))
        'error (or (turn-err tn) (json-null))))

(define (marker->jsexpr m)
  (hash 'type (marker-type m)
        'message (or (marker-message m) (json-null))))

;; A consistent copy, safe to read while a turn is streaming: the structs stay
;; behind the lock and what leaves is plain JSON values.
(define (agent-transcript ag)
  (with-state ag
    (λ ()
      (for/list ([e (in-list (reverse (acp-agent-entries ag)))])
        (if (turn? e) (turn->jsexpr e) (marker->jsexpr e))))))

(define (agent-busy? ag)
  (with-state ag (λ () (and (acp-agent-busy? ag) #t))))

;; ---- which model ------------------------------------------------------------
;;
;; The agent reports its model as one of the session's CONFIG OPTIONS: the
;; entry with id "model" carries the live model in `currentValue`, and the
;; picker it came from in `options`. It arrives twice over — in the session/new
;; result, and again in a `config_option_update` whenever anything in that set
;; moves — so one reader serves both, and nothing here ever guesses a name.

(define model-config-id "model")

(define (agent-model ag)
  (with-state ag (λ () (acp-agent-model ag))))

;; The picker's own label ("Fable", "Opus") is what the agent calls the model,
;; and it is what a header wants; the raw id is the fallback for a session
;; running something the picker no longer offers.
(define (config-options-model opts)
  (and (list? opts)
       (for/or ([o (in-list opts)]
                #:when (hash? o))
         (and (equal? (hash-ref o 'id #f) model-config-id)
              (let ([current (string-or-false (hash-ref o 'currentValue #f))])
                (and current
                     (or (option-name (hash-ref o 'options '()) current)
                         current)))))))

(define (option-name options value)
  (and (list? options)
       (for/or ([o (in-list options)]
                #:when (hash? o))
         (and (equal? (hash-ref o 'value #f) value)
              (string-or-false (hash-ref o 'name #f))))))

;; Learned, never configured. The frame goes out only when the name actually
;; moved: a session that reports the same model on every reset would otherwise
;; tell every panel the same thing over and over.
(define (learn-model! ag opts)
  (define name (config-options-model opts))
  (when name
    (with-state ag
      (λ ()
        (unless (equal? name (acp-agent-model ag))
          (set-acp-agent-model! ag name)
          (broadcast! ag (hash 'type "model" 'name name)))))))

;; ---- the wire --------------------------------------------------------------

(define (send-line! ag js)
  (define line (jsexpr->string js))
  (call-with-semaphore
   (acp-agent-out-sema ag)
   (λ ()
     (define out (acp-agent-stdin ag))
     (unless out (user-fail "the agent is not running"))
     (write-string line out)
     (write-string "\n" out)
     (flush-output out))))

(define (next-id! ag)
  (with-state ag
    (λ ()
      (define id (add1 (acp-agent-next-id ag)))
      (set-acp-agent-next-id! ag id)
      id)))

;; Every outstanding request owns a channel; the reader puts the answer there,
;; and a subprocess that died puts a failure into all of them at once. Nobody
;; waits on a process that is gone.
(define (register-pending! ag id ch)
  (with-state ag
    (λ () (set-acp-agent-pending! ag (hash-set (acp-agent-pending ag) id ch)))))

(define (forget-pending! ag id)
  (with-state ag
    (λ () (set-acp-agent-pending! ag (hash-remove (acp-agent-pending ag) id)))))

(define (fail-all-pending! ag message)
  (define pending
    (with-state ag
      (λ ()
        (define p (acp-agent-pending ag))
        (set-acp-agent-pending! ag (hash))
        p)))
  (for ([(_id ch) (in-hash pending)])
    (async-channel-put ch (cons 'error message))))

;; -> the result jsexpr. Raises when the agent answered with an error, said
;; nothing before the deadline, or died with the question outstanding.
;;
;; `after-send` runs once the request is on the wire and before the wait —
;; the only moment from which a follow-up notification about THIS request is
;; meaningful to the agent (see the pending cancel).
(define (request! ag method params
                  #:timeout [timeout boot-timeout-seconds]
                  #:after-send [after-send void])
  (define id (next-id! ag))
  (define ch (make-async-channel))
  (register-pending! ag id ch)
  (define answer
    (with-handlers ([exn:fail? (λ (e) (forget-pending! ag id) (raise e))])
      (send-line! ag (hash 'jsonrpc "2.0" 'id id 'method method 'params params))
      (after-send)
      (if timeout (sync/timeout timeout ch) (sync ch))))
  (forget-pending! ag id)
  (cond
    [(not answer) (user-fail "~a: no answer from the agent in ~as" method timeout)]
    [(eq? (car answer) 'error) (user-fail "~a: ~a" method (cdr answer))]
    [else (cdr answer)]))

(define (notify! ag method params)
  (send-line! ag (hash 'jsonrpc "2.0" 'method method 'params params)))

;; ---- reading the agent -----------------------------------------------------

(define (reader-loop ag in)
  (let loop ()
    (define line
      (with-handlers ([exn:fail? (λ (_e) eof)])
        (read-line in 'any)))
    (cond
      [(eof-object? line) (handle-eof! ag)]
      [(string=? (string-trim line) "") (loop)]
      [else
       (define js (with-handlers ([exn:fail? (λ (_e) #f)]) (string->jsexpr line)))
       ;; A line that is not a JSON-RPC object is the agent's problem, not the
       ;; stream's: say so and keep reading.
       (if (hash? js)
           (dispatch! ag js)
           (log-line ag (format "dropped a line that is not JSON-RPC: ~a" (elide line))))
       (loop)])))

(define (dispatch! ag js)
  (define method (hash-ref js 'method #f))
  (define id (hash-ref js 'id #f))
  (with-handlers ([exn:fail? (λ (e) (log-line ag (format "dispatch failed: ~a" (exn-message e))))])
    (cond
      [(and method id) (handle-request! ag id method (hash-ref js 'params (hash)))]
      [method (handle-notification! ag method (hash-ref js 'params (hash)))]
      [id (handle-response! ag id js)]
      [else (log-line ag "dropped a JSON-RPC object with no id and no method")])))

;; JSON-RPC says a reply carries `result` or `error`; some implementations
;; carry both, with one of them null. The object is the one that decides.
(define (handle-response! ag id js)
  (define ch (hash-ref (with-state ag (λ () (acp-agent-pending ag))) id #f))
  (define err (hash-ref js 'error #f))
  (define answer
    (if (hash? err)
        (cons 'error (let ([m (hash-ref err 'message #f)])
                       (if (string? m) m (jsexpr->string err))))
        (cons 'ok (hash-ref js 'result (hash)))))
  (if ch
      (async-channel-put ch answer)
      (log-line ag (format "answer to a question nobody asked (id ~a)" id))))

;; ---- incoming requests -----------------------------------------------------

;; The agent asks the client for things. Only one of them matters here, and it
;; matters because NOT answering hangs the turn.
(define (handle-request! ag id method params)
  (cond
    [(equal? method "session/request_permission")
     (define picked (allow-option (hash-ref params 'options '())))
     (send-line! ag (hash 'jsonrpc "2.0" 'id id
                          'result (hash 'outcome
                                        (if picked
                                            (hash 'outcome "selected" 'optionId picked)
                                            (hash 'outcome "cancelled")))))]
    [else
     (send-line! ag (hash 'jsonrpc "2.0" 'id id
                          'error (hash 'code -32601
                                       'message (format "method not found: ~a" method))))]))

;; The first option whose kind says yes (allow_once / allow_always), in the
;; order the agent offered them. Never a rejection: a chat panel with no
;; permission UI that denies is a chat panel that fails every tool call.
(define (allow-option options)
  (and (list? options)
       (for/or ([o (in-list options)]
                #:when (hash? o))
         (define kind (hash-ref o 'kind ""))
         (and (string? kind)
              (string-prefix? kind "allow")
              (let ([oid (hash-ref o 'optionId #f)])
                (and (string? oid) oid))))))

;; ---- incoming notifications ------------------------------------------------

(define (handle-notification! ag method params)
  (cond
    [(equal? method "session/update") (handle-update! ag params)]
    [else (log-kind-once! ag method)]))

;; Update kinds this version consumes. Thoughts, plans and the rest of ACP's
;; running commentary are deliberately dropped — a chat panel that renders
;; everything an agent thinks is a log viewer.
(define ignored-update-kinds
  '("user_message_chunk" "agent_thought_chunk" "plan"))

(define (handle-update! ag params)
  (define u (hash-ref params 'update (hash)))
  (define kind (and (hash? u) (hash-ref u 'sessionUpdate #f)))
  (cond
    [(equal? kind "agent_message_chunk")
     (define text (content-text (hash-ref u 'content (hash))))
     (when (non-empty-string? text) (chunk! ag text))]
    [(equal? kind "tool_call")
     (tool-call! ag
                 (hash-ref u 'toolCallId #f)
                 (or (string-or-false (hash-ref u 'title #f))
                     (string-or-false (hash-ref u 'kind #f))
                     "tool")
                 (or (string-or-false (hash-ref u 'status #f)) "pending"))]
    [(equal? kind "tool_call_update")
     (tool-update! ag
                   (hash-ref u 'toolCallId #f)
                   (string-or-false (hash-ref u 'title #f))
                   (string-or-false (hash-ref u 'status #f)))]
    ;; the whole config set, resent: only the model is read out of it
    [(equal? kind "config_option_update")
     (learn-model! ag (hash-ref u 'configOptions '()))]
    [(member kind ignored-update-kinds) (void)]
    [else (log-kind-once! ag (format "session/update ~a" kind))]))

(define (string-or-false v) (and (string? v) v))

;; A content block, as much of it as a chat line can show.
(define (content-text c)
  (cond
    [(and (hash? c) (equal? (hash-ref c 'type #f) "text"))
     (define t (hash-ref c 'text ""))
     (if (string? t) t "")]
    [else ""]))

;; Noise the bridge does not understand is worth exactly one line of log each.
(define (log-kind-once! ag kind)
  (define new?
    (with-state ag
      (λ ()
        (cond
          [(hash-ref (acp-agent-seen-kinds ag) kind #f) #f]
          [else (set-acp-agent-seen-kinds! ag (hash-set (acp-agent-seen-kinds ag) kind #t))
                #t]))))
  (when new? (log-line ag (format "ignoring ~a" kind))))

;; ---- turn bookkeeping ------------------------------------------------------

(define (chunk! ag text)
  (with-state ag
    (λ ()
      (define tn (acp-agent-live-turn ag))
      (cond
        [tn
         (set-turn-agent! tn (string-append (turn-agent tn) text))
         (broadcast! ag (hash 'type "chunk" 'text text))]
        [else (log-line ag "text arrived outside a turn; dropped")]))))

(define (tool-call! ag id title status)
  (when (string? id)
    (with-state ag
      (λ ()
        (define tn (acp-agent-live-turn ag))
        (when tn
          (define t (or (find-tool tn id)
                        (let ([t (tool id title status)])
                          (set-turn-tools! tn (cons t (turn-tools tn)))
                          t)))
          (set-tool-title! t title)
          (set-tool-status! t status)
          (broadcast! ag (hash 'type "tool" 'id id 'title title 'status status)))))))

;; An update carries only what changed; the line keeps whatever it had.
(define (tool-update! ag id title status)
  (when (string? id)
    (with-state ag
      (λ ()
        (define tn (acp-agent-live-turn ag))
        (define t (and tn (find-tool tn id)))
        (cond
          [t
           (when title (set-tool-title! t title))
           (when status (set-tool-status! t status))
           (broadcast! ag (hash 'type "tool" 'id id
                                'title (tool-title t) 'status (tool-status t)))]
          [tn
           ;; an update for a call we never saw: still a line worth showing
           (define t2 (tool id (or title "tool") (or status "in_progress")))
           (set-turn-tools! tn (cons t2 (turn-tools tn)))
           (broadcast! ag (hash 'type "tool" 'id id
                                'title (tool-title t2) 'status (tool-status t2)))]
          [else (void)])))))

(define (find-tool tn id)
  (for/or ([t (in-list (turn-tools tn))])
    (and (equal? (tool-id t) id) t)))

;; ---- the subprocess --------------------------------------------------------

(define (alive? ag)
  (define sp (acp-agent-sp ag))
  (and sp (eq? (subprocess-status sp) 'running)))

;; Spawned with the environment as it stands: HOME is where the adapter's
;; credentials live, and the nix wrapper has already set the rest.
(define (spawn! ag)
  (define-values (sp out in err)
    (in-custodian ag
      (λ ()
        (parameterize ([current-directory (acp-agent-cwd ag)])
          (subprocess #f #f #f (acp-agent-command ag))))))
  (define restart?
    (with-state ag
      (λ ()
        (set-acp-agent-sp! ag sp)
        (set-acp-agent-stdin! ag in)
        (set-acp-agent-stdout! ag out)
        (set-acp-agent-session! ag #f)
        (begin0 (acp-agent-spawned? ag)
          (set-acp-agent-spawned?! ag #t)))))
  (when restart? (log-line ag "restarted the agent"))
  (in-custodian ag (λ () (thread (λ () (reader-loop ag out)))))
  (in-custodian ag (λ () (thread (λ () (drain-log ag err)))))
  (handshake! ag)
  (new-session! ag))

(define (drain-log ag err)
  (let loop ()
    (define line (with-handlers ([exn:fail? (λ (_e) eof)]) (read-line err 'any)))
    (unless (eof-object? line)
      (log-line ag line)
      (loop))))

(define (handshake! ag)
  (request! ag "initialize"
            (hash 'protocolVersion protocol-version
                  'clientCapabilities client-capabilities))
  (void))

;; A session is a conversation's memory. set_mode is asked for but not
;; required: it is refused when the server runs as root, and the permission
;; auto-answer covers that case.
(define (new-session! ag)
  (define r (request! ag "session/new"
                      (hash 'cwd (path->string (acp-agent-cwd ag))
                            'mcpServers '())))
  (define sid (hash-ref r 'sessionId #f))
  (unless (string? sid) (user-fail "session/new returned no sessionId"))
  (with-state ag (λ () (set-acp-agent-session! ag sid)))
  ;; the session says what it runs; a bridge that asked would be guessing
  (learn-model! ag (hash-ref r 'configOptions '()))
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-line ag (format "set_mode ~a refused: ~a" bypass-mode (exn-message e)))
                     (with-state ag
                       (λ () (broadcast! ag (hash 'type "error"
                                                  'message (format "permission mode ~a refused; \
tool calls are allowed one at a time instead"
                                                                   bypass-mode))))))])
    (request! ag "session/set_mode" (hash 'sessionId sid 'modeId bypass-mode)))
  sid)

;; Spawn on demand, and only once: callers hold nothing while this runs.
(define (ensure-session! ag)
  (call-with-semaphore
   (acp-agent-boot-sema ag)
   (λ ()
     (cond
       [(not (with-state ag (λ () (alive? ag)))) (spawn! ag)]
       [(not (with-state ag (λ () (acp-agent-session ag)))) (new-session! ag)]
       [else (void)])
     (with-state ag (λ () (acp-agent-session ag))))))

;; The agent's stdout ended: it exited, or it is about to. Whoever was waiting
;; on an answer gets a failure instead, the live turn (if any) ends in the
;; thread that owns it, and the NEXT prompt spawns a fresh one. No respawn
;; loop: nothing here starts a process nobody asked for.
(define (handle-eof! ag)
  (define sp (with-state ag (λ () (acp-agent-sp ag))))
  (when sp (sync/timeout stop-timeout-seconds sp))
  (define status (and sp (subprocess-status sp)))
  (define stopped? (with-state ag (λ () (acp-agent-stopped? ag))))
  (define why
    (if (integer? status)
        (format "the agent exited (code ~a)" status)
        "the agent closed its connection"))
  (with-state ag
    (λ ()
      (set-acp-agent-sp! ag #f)
      (set-acp-agent-stdin! ag #f)
      (set-acp-agent-stdout! ag #f)
      (set-acp-agent-session! ag #f)
      (unless stopped?
        (push-entry! ag (marker "restart"
                                (string-append why "; the next prompt starts a new session"))))))
  (fail-all-pending! ag why))

;; ---- prompting -------------------------------------------------------------

(define (busy-fail)
  (raise (exn:fail:op "the agent is busy with another turn"
                      (current-continuation-marks)
                      'busy #f #f #f)))

(define (stopped-fail)
  (raise (exn:fail:op "the agent has been stopped"
                      (current-continuation-marks)
                      'validation #f #f #f)))

;; Non-blocking: the turn is accepted (or refused) here and runs in its own
;; thread. The echoed `user` frame is what makes a second browser tab show the
;; message this one typed.
(define (agent-prompt! ag text)
  (define tn
    (with-state ag
      (λ ()
        (when (acp-agent-stopped? ag) (stopped-fail))
        (when (acp-agent-busy? ag) (busy-fail))
        (define tn (turn text "" '() 'running #f #f #f))
        ;; a cancel left over from a turn that died before it could be sent
        ;; belongs to that turn, not this one
        (set-acp-agent-cancel-pending?! ag #f)
        (set-acp-agent-busy?! ag #t)
        (set-acp-agent-live-turn! ag tn)
        (push-entry! ag tn)
        (broadcast! ag (hash 'type "user" 'text text))
        tn)))
  (in-custodian ag (λ () (thread (λ () (run-turn ag tn)))))
  (void))

(define (run-turn ag tn)
  (define outcome
    (with-handlers ([exn:fail? (λ (e) (cons 'error (exn-message e)))])
      (define sid (ensure-session! ag))
      (cons 'ok (request! ag "session/prompt"
                          (hash 'sessionId sid
                                'prompt (list (hash 'type "text" 'text (turn-text tn))))
                          #:timeout #f
                          #:after-send (λ () (flush-cancel! ag tn sid))))))
  (with-state ag
    (λ ()
      (cond
        [(eq? (car outcome) 'ok)
         (define stop (let ([s (hash-ref (cdr outcome) 'stopReason "end_turn")])
                        (if (string? s) s "end_turn")))
         (set-turn-status! tn 'done)
         (set-turn-stop! tn stop)
         ;; Markdown is render-time only: the turn keeps the raw text, and the
         ;; frame carries a rendered COPY so a panel that accumulated plain
         ;; chunks can swap in the real thing without parsing anything.
         (broadcast! ag (hash 'type "done" 'stopReason stop
                              'html (note->html-string (turn-agent tn))))]
        [else
         (set-turn-status! tn 'error)
         (set-turn-err! tn (cdr outcome))
         (broadcast! ag (hash 'type "error" 'message (cdr outcome)))])
      (set-acp-agent-live-turn! ag #f)
      (set-acp-agent-busy?! ag #f))))

;; A notification, not a request: the turn does not end here, it ends when the
;; session/prompt response comes back saying "cancelled".
;;
;; A turn is accepted before it is sent — spawning the agent and shaking hands
;; takes seconds, and a person who types and immediately hits stop is inside
;; that window. There is no session to name yet, and a session/cancel that
;; arrives before the prompt names a turn the agent has never heard of, so the
;; cancel is REMEMBERED and sent by run-turn the moment the prompt is on the
;; wire. Either way the turn ends the one way it can: a `done` frame with
;; stopReason cancelled.
(define (agent-cancel! ag)
  (define sid
    (with-state ag
      (λ ()
        (define tn (acp-agent-live-turn ag))
        (define sid (acp-agent-session ag))
        (cond
          [(not (and (acp-agent-busy? ag) tn)) #f]
          [(and (turn-sent? tn) sid) sid]
          [else (set-acp-agent-cancel-pending?! ag #t) #f]))))
  (when sid (send-cancel! ag sid))
  (void))

;; The prompt is on the wire. Mark it, and take any cancel that was waiting
;; for exactly this moment.
(define (flush-cancel! ag tn sid)
  (define pending
    (with-state ag
      (λ ()
        (set-turn-sent?! tn #t)
        (begin0 (acp-agent-cancel-pending? ag)
          (set-acp-agent-cancel-pending?! ag #f)))))
  (when pending (send-cancel! ag sid)))

;; A cancel the agent cannot hear is not worth failing a caller over: the turn
;; is ending regardless (the reader will see the prompt fail, or EOF).
(define (send-cancel! ag sid)
  (with-handlers ([exn:fail? (λ (e) (log-line ag (format "cancel failed: ~a" (exn-message e))))])
    (notify! ag "session/cancel" (hash 'sessionId sid)))
  (void))

;; Wait for the live turn to settle. -> #t when it did.
(define (wait-idle ag seconds)
  (define deadline (+ (current-inexact-milliseconds) (* 1000.0 seconds)))
  (let loop ()
    (cond
      [(not (agent-busy? ag)) #t]
      [(>= (current-inexact-milliseconds) deadline) #f]
      [else (sleep 0.02) (loop)])))

;; New chat: the agent-side context goes away, the transcript keeps its
;; history and says where the break is. A turn in flight is cancelled first —
;; and if it will not settle, the subprocess is taken away from it, which ends
;; it with an error frame rather than leaving the panel wedged.
(define (agent-reset! ag)
  (when (agent-busy? ag)
    (agent-cancel! ag)
    (unless (wait-idle ag reset-timeout-seconds)
      (log-line ag "a turn would not settle; taking the subprocess away from it")
      (kill-subprocess! ag)
      (wait-idle ag reset-timeout-seconds)))
  (with-handlers ([exn:fail?
                   (λ (e)
                     ;; No session to hand back: the next prompt spawns one.
                     (log-line ag (format "reset could not start a session: ~a" (exn-message e)))
                     (with-state ag (λ () (set-acp-agent-session! ag #f))))])
    (call-with-semaphore
     (acp-agent-boot-sema ag)
     (λ () (when (with-state ag (λ () (alive? ag))) (new-session! ag)))))
  (with-state ag
    (λ ()
      (push-entry! ag (marker "reset" #f))
      (broadcast! ag (hash 'type "reset"))))
  (void))

(define (kill-subprocess! ag)
  (define sp (with-state ag (λ () (acp-agent-sp ag))))
  (when sp
    (with-handlers ([exn:fail? void]) (subprocess-kill sp #t))
    (sync/timeout stop-timeout-seconds sp))
  (void))

;; Closing its stdin is the documented way out: the adapter exits 0 on EOF.
;; Idempotent — the server's stop procedure may run more than once, and a
;; process that already left needs nothing.
(define (agent-stop! ag)
  (define sp
    (with-state ag
      (λ ()
        (set-acp-agent-stopped?! ag #t)
        (acp-agent-sp ag))))
  (when sp
    (define in (with-state ag (λ () (acp-agent-stdin ag))))
    (when in (with-handlers ([exn:fail? void]) (close-output-port in)))
    (unless (sync/timeout stop-timeout-seconds sp)
      (with-handlers ([exn:fail? void]) (subprocess-kill sp #t))
      (sync/timeout stop-timeout-seconds sp))
    (with-state ag
      (λ ()
        (set-acp-agent-sp! ag #f)
        (set-acp-agent-stdin! ag #f)
        (set-acp-agent-session! ag #f))))
  (void))
