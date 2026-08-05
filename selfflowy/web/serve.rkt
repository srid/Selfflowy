#lang racket/base

;; The read-mostly web view.
;;
;;   GET /              the html page: sidebar + outline
;;   GET /today         today's Daily day node, zoomed
;;   GET /events        SSE stream; `outline` (data: store revision) per reload
;;   GET /api/tree      byte-identical to `selfflowy tree`
;;   GET /api/agenda    byte-identical to `selfflowy agenda --json`
;;   GET /static/*      files from web/static/
;;   anything else      404, terse text/plain
;;
;; No auth: the network is the auth (Tailscale / Caddy in front of it).
;; Routing, static files, and MIME types come from racket web-server. Outline
;; content comes from selfflowy/store — this module owns routes and responses,
;; never a load.
;;
;; Live updates are three parts that only meet here: the store knows WHAT the
;; outlines are, the watcher knows WHEN they moved, the hub knows WHO is
;; listening. None of them knows about the other two. The ACP bridge is a
;; fourth of the same kind — it pushes `chat` through the same hub and has
;; never heard of HTTP; the routes that drive it are the next work package.

(require racket/async-channel
         racket/path
         (for-syntax racket/base)
         json
         net/url
         racket/runtime-path
         web-server/web-server
         web-server/http
         web-server/dispatch
         web-server/dispatchers/dispatch
         web-server/dispatchers/filesystem-map
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (only-in web-server/private/mime-types make-path->mime-type)
         selfflowy/agenda
         selfflowy/dates
         selfflowy/json/model
         selfflowy/json/reply
         selfflowy/load
         (only-in selfflowy/paths file-label roots-base)
         selfflowy/store
         selfflowy/web/acp
         selfflowy/web/events
         selfflowy/web/render
         selfflowy/web/watch)

(provide start-server)

;; static files: render owns the directory and the URL prefix (it also
;; writes the <head> that links them); this module only mounts them.
(define-runtime-path mime-types-path
  (list 'lib "default-web-root/mime.types" "web-server"))

;; ---- responses ------------------------------------------------------------

(define (html-response str #:code [code 200])
  (response/output
   (λ (out) (write-string str out))
   #:code code
   #:mime-type #"text/html; charset=utf-8"))

;; write-json + newline: the same bytes the CLI writes to stdout.
(define (json-response js #:code [code 200])
  (response/output
   (λ (out)
     (write-json js out)
     (newline out))
   #:code code
   #:mime-type #"application/json; charset=utf-8"))

(define (text-response str #:code [code 200])
  (response/output
   (λ (out) (write-string str out))
   #:code code
   #:mime-type #"text/plain; charset=utf-8"))

(define (not-found-response)
  (text-response "404 not found\n" #:code 404))

;; ---- the store ------------------------------------------------------------

;; Every route starts here: refresh the store (a cheap mtime probe unless a
;; file actually changed), then hand the handler ONE consistent snapshot.
;;
;; A live load error means the file is mid-edit. JSON routes fail loudly —
;; agents must never be handed stale data quietly — while the page keeps the
;; last good content and shows the error in its banner (#:stale-ok? #t). With
;; no last-good snapshot at all, everything fails.
(define (with-snapshot st fail proc #:stale-ok? [stale-ok? #f])
  (store-invalidate! st)
  (define snap (store-snapshot st))
  (define err (store-error st))
  (if (and err (or (not stale-ok?) (null? (snapshot-outlines snap))))
      (fail err)
      (proc snap err)))

(define (load-error->json err)
  (err-hash (load-error-message err)
            #:file (load-error-file err)
            #:line (load-error-line err)
            #:col (load-error-col err)))

(define (json-failure err)
  (json-response (load-error->json err) #:code 500))

(define (error-banner err)
  (render-error-banner (load-error-detail err) #:where (load-error-where err)))

;; Nothing to show at all — the FIRST load failed. Still an SSE page: the
;; next save is what fixes it, and the client should not have to reload to
;; find that out.
(define (page-failure err #:live-href live-href)
  (html-response
   (page->html-string
    (render-page (render-empty-pane "No outline loaded." #:home-href home-href)
                 #:title "selfflowy"
                 #:banner (error-banner err)
                 #:sse-connect events-href
                 #:live-href live-href))
   #:code 500))

;; ---- the route table ------------------------------------------------------
;;
;; One owner: these are the only URLs the app has, and the renderer is told
;; them rather than guessing (it used to default to a /today that did not
;; exist, so the shipped sidebar link 404'd).

(define home-href "/")
(define today-href "/today")

;; The push channel. A page re-fetches ITSELF on an `outline` event, so the
;; href it re-fetches is whichever of the two above rendered it — handed to
;; the renderer, never guessed by it.
(define events-href "/events")

;; A node's address. There is no zoom route yet, so it is the home page
;; anchored at the node — every node carries id="n-<key>" there.
(define node-href-base "/#n-")

;; ---- handlers -------------------------------------------------------------

(define (page-title files)
  (if (= (length files) 1)
      (file-label (car files))
      "selfflowy"))

(define (chrome files-data main
                #:title title
                #:live-href live-href
                #:banner [banner #f]
                #:code [code 200])
  (html-response
   (page->html-string
    (render-page main
                 #:title title
                 #:sidebar (render-sidebar files-data
                                           #:home-href home-href
                                           #:today-href today-href
                                           #:zoom-base node-href-base)
                 #:banner banner
                 #:sse-connect events-href
                 #:live-href live-href))
   #:code code))

(define (page-handler st)
  (with-snapshot st (λ (err) (page-failure err #:live-href home-href)) #:stale-ok? #t
    (λ (snap err)
      (define files-data (snapshot-files-data snap))
      (chrome files-data
              (render-outline files-data #:today (today-iso-string))
              #:title (page-title (store-files st))
              #:live-href home-href
              #:banner (and err (error-banner err))))))

;; Today's Daily day node, zoomed. No day node yet is the normal state before
;; the first capture of the day, not an error.
(define (today-handler st)
  (with-snapshot st (λ (err) (page-failure err #:live-href today-href)) #:stale-ok? #t
    (λ (snap err)
      (define today (today-iso-string))
      (define key (snapshot-day-key snap today))
      (chrome (snapshot-files-data snap)
              (if key
                  (render-zoom (snapshot-index snap) key
                               #:today today
                               #:home-href home-href
                               #:zoom-base node-href-base)
                  (render-empty-pane
                   (format "No day node for ~a. Run: selfflowy daily" today)
                   #:home-href home-href))
              #:title (string-append "today " today)
              #:live-href today-href
              #:banner (and err (error-banner err))))))

(define (tree-handler st)
  (with-snapshot st json-failure
    (λ (snap _err) (json-response (outlines->jsexpr (snapshot-outlines snap))))))

(define (agenda-handler st)
  (with-snapshot st json-failure
    (λ (snap _err)
      (define today (today-iso-string))
      (define groups
        (agenda-groups-from-files
         (for/list ([o (in-list (snapshot-outlines snap))])
           (cons (outline-path o) (outline-tasks o)))
         today))
      (json-response (agenda-groups->jsexpr groups today)))))

;; ---- dispatch -------------------------------------------------------------

(define (make-router st hub)
  (define-values (route _url)
    (dispatch-rules
     [("") (λ (req) (page-handler st))]
     [("today") (λ (req) (today-handler st))]
     ;; mounted, not understood: what an event MEANS lives in web/events
     [("events") (λ (req) (hub-response hub))]
     [("api" "tree") (λ (req) (tree-handler st))]
     [("api" "agenda") (λ (req) (agenda-handler st))]
     [else (λ (req) (not-found-response))]))
  route)

;; /static/foo.css -> the render collection's static/foo.css. make-url->path
;; refuses anything that climbs out of the base ("/static/../..") — we turn
;; that into a plain 404 instead of an error page.
(define static-url->path
  (let ([u->p (make-url->path (web-static-dir))])
    (λ (u)
      (define rest (if (pair? (url-path u)) (cdr (url-path u)) '()))
      (with-handlers ([exn:fail? (λ (_e) (next-dispatcher))])
        (u->p (struct-copy url u [path rest]))))))

(define (make-dispatcher st hub)
  (sequencer:make
   (filter:make (regexp (string-append "^" (regexp-quote web-static-prefix)))
                (files:make #:url->path static-url->path
                            #:path->mime-type (make-path->mime-type mime-types-path)
                            #:indices '()))
   (lift:make (make-router st hub))))

;; ---- server ---------------------------------------------------------------

;; Returns a stop procedure. #:on-listen gets the port actually bound (useful
;; when #:port is 0, i.e. "pick one").
;;
;; #:acp-command is the agent `serve` chats with — #f means there is none, and
;; the CLI never passes #f (it refuses to start without one; see docs/cli.md).
;; #:on-agent is handed the bridge once it exists: the seam for tests, and for
;; anything that wants to prompt the agent without an HTTP request.
(define (start-server #:port [port 8080]
                      #:bind [bind "127.0.0.1"]
                      #:files files
                      #:acp-command [acp-command #f]
                      #:on-listen [on-listen void]
                      #:on-agent [on-agent void])
  (define st (make-store files))
  (define hub (make-hub))
  ;; The agent's working directory is the outlines' own: one file means its
  ;; directory, several mean the deepest one that holds them all (the same
  ;; base node keys are minted against). Nothing is spawned here — the bridge
  ;; starts a subprocess on the first prompt.
  (define agent
    (and acp-command
         (make-acp-agent #:command acp-command
                         #:cwd (roots-base files)
                         #:broadcast (λ (name data) (hub-broadcast! hub name data)))))
  (define confirm (make-async-channel 1))
  (define stop
    (serve #:dispatch (make-dispatcher st hub)
           #:port port
           #:listen-ip bind
           #:confirmation-channel confirm))
  (define bound (async-channel-get confirm))
  (when (exn? bound)
    (stop)
    (raise bound))
  ;; Only once there is a listener: a watcher with nobody to tell is a
  ;; thread that reloads outlines for its own amusement.
  ;;
  ;; The payload is the store revision — the browser only needs "re-fetch",
  ;; but a revision makes the stream readable by hand (curl) and gives a
  ;; client something to compare.
  (define stop-watcher
    (start-watcher st
                   #:on-change
                   (λ () (hub-broadcast! hub "outline"
                                         (number->string (store-revision st))))))
  (when agent (on-agent agent))
  (on-listen bound)
  (λ ()
    (stop-watcher)
    (when agent (agent-stop! agent))
    (stop)))
