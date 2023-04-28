;;;; Copyright 2014 Vibhu Mohindra
;;;; Licence: GNU AGPL 3

; this file was copied from my afas program.
; it reached there from my bitcoin arb program.
; changed package line. removed unnecesssary comments.

(in-package "BTCPAY")

;non-pipelined http client that keeps connections alive
;sends full drakma header set, which is unnecessary traffic
;so the only thing this client does is:
;  keep connections alive
;  reconnect them if they are disconnected
;  retry the failed request when it does that

(def (close2 stream1)
  (if stream1 (ignore-errors (close stream1))))
(def (make-ka-conn) (list nil))
(def (close-ka-conn conn) (close2 (car conn)))

;;tries >= 1
;;you should pass in :keep-alive t if you think you may encounter
;;http/1.0 servers
(def (http-ka-request2 tries conn http-request-args)
  "Returned list's car is nil whenever Drakma returns a nil or all tries
   produce errors. Drakma may return nil whenever Content-Length is 0."
  (do ((i 0 (1+ i)))
      ((>= i tries) '(nil))
    (let ((resp (handler-case
		 (multiple-value-list
		  (apply http-request (append http-request-args
					      (list :close nil
						    :stream (car conn)))))
		 (error (e) (list :error e)))))
      (cond ((eq (car resp) :error)
             (displayln "http-ka closing due to error:" (cadr resp))
             (close2 (car conn))
             (setf (car conn) nil))
            ((nth 5 resp)
	     (displayln "http-ka closing on command~%")
	     (close2 (nth 4 resp))
	     (setf (car conn) nil))
            (else (setf (car conn) (nth 4 resp))))
      (unless (eq (car resp) :error)
        (return resp)))))

(def (http-ka-request conn . http-request-args)
  (http-ka-request2 2 conn http-request-args))
