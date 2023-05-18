;; Keep-Alive connections for Gerbil
;;
;; This file abstracts over the need to:
;; 1. use a keepalive connection to an HTTP server if available (spoiler: not at the moment)
;; 2. Reconnect to the HTTP server if no keepalive connection is available.
;; 3. Retry a HTTP request if it fails for low-level reasons
;;
;; TODO: actually add keepalive connections to :std/net/request or some lower level API.

;; NB:
;; Drakma returns => body-or-stream0, status-code1, headers2, uri3, stream4, must-close5, reason-phrase6
;; a req is => port url history status status-text headers body encoding
;; does the drakma stream indeed correspond to the scheme port?


(export #t) ;; TODO: refine the list of exports

(import :std/iter :std/sugar
        :std/net/request)

;; TODO: either generate URL from server and port, or extract server and port from URL
(defstruct KeepaliveConnection
  (server ;; : String
   port ;; : (RangedInteger 0 65536)
   connection) ;; : (OrFalse Connection) ;; -- always false for now (!)
  transparent: #t
  constructor: :init!)

(defmethod {:init! KeepaliveConnection}
  (lambda (self server port) (struct-instance-init! self server port #f)))

(defmethod {close KeepaliveConnection}
  (lambda (self)
    (def c (KeepaliveConnection-connection self))
    (when c {close c})))

(defmethod {request KeepaliveConnection}
  (lambda (self method url
           retries:  (retries 2)
           redirect: (redirect #t)
           headers:  (headers #f)
           cookies:  (cookies #f)
           params:   (params #f)
           data:     (data #f)
           auth:     (auth #f))
    ;; We can directly use http-any from std/net/request after https://github.com/vyzo/gerbil/pull/705
    (def (http-any method url . args)
      (apply (case method
               ((GET) http-get)
               ((HEAD) http-head)
               ((POST) http-post)
               ((PUT) http-put)
               ((DELETE) http-delete)
               ((OPTIONS) http-options)
               (else (error "Unknown HTTP method" method)))
        url args))
    ;; NOTE: for now, we always create a new connection, since request doesn't support keepalive yet.
    ;; We also do the retries for trivial errors
    (let/cc return
      (def err #f)
      (for (_ (in-range retries))
        (try
         (return (http-any method url
                           redirect: redirect headers: headers cookies: cookies
                           params: params data: data auth: auth))
         (catch (e) (set! err e))))
      (raise err))))
