;; the commands are a good place to strictly sanitise/typecheck all args.
(import
  :std/assert :std/iter :std/misc/hash :std/misc/list :std/srfi/1 :std/sugar
  :clan/timestamp
  ./keys ./scripts)

#|
(def (cmd-register-keyspace descriptor)
  (assert (stringp descriptor))
  (log-change `(register-keyspace ',descriptor))
  (register-keyspace descriptor))

(def (cmd-create-invoice keyspace-id custid invid amount-str)
  (assert! (string? amount-str))
  (assert! (hash-key? descriptor<-keyspace keyspace-id))
  (assert! (and (string? custid) (string? invid))) ;for compat with slack app
  (let ((now (current-unix-timestamp)))
    (log-change
      `(create-invoice2 'cli ',now ',keyspace-id ',custid ',invid ',amount-str))
    (create-invoice2 'cli now keyspace-id custid invid amount-str)))

(def (cmd-cli-process-announcements )
  (when (hash-get *parted-announcements* "cli") ; to not clutter changelog.
    (begin0
      (cli-process-announcements)
      ;log only after success, so you re-announce on crash recovery
      (log-change-lax '(cli-process-announcements)))))


(defvar *announcement-stream* *standard-output*)

(def (cli-process-announcements )
  (let ((entry (hash-get *parted-announcements* "cli")))
    (when entry
      (writeln (announcements-ht-to-alist (announcements-to-ht entry))
               *announcement-stream*)
      (finish-output *announcement-stream*) ;must, as might log success soon
      (hash-put! *parted-announcements* "cli" '()))))

(def (announcements-to-ht announcements)
  (def ann (hash)) ;keyspace to custid to invs
  (for (a announcements)
    (def qc (car a))
    (def ht (hash-ensure-ref ann (qcust-keyspace qc) (cut hash)))
    (hash-put! ht (qcust-custid qc)
               (append-reverse (cdr a) (hash-ref ht (qcust-custid qc) '()))))
  ann)

;; returns nested alist: keyspace to custid to invs
(def (announcements-ht-to-alist ann-ht)
  (let (results '())
    (hash-map
     (lambda (keyspace ht)
       (def alist '())
       (hash-map (lambda (custid invs)
                   (push! (cons custid invs) alist) ht))
                   (push! (cons keyspace alist) results))
     ann-ht)
    results))
|#
