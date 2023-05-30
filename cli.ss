;; the commands are a good place to strictly sanitise/typecheck all args.
(import
  ./scripts)

#|
(def (cmd-register-keyspace descriptor)
  (assert (stringp descriptor))
  (log-change `(register-keyspace ',descriptor))
  (register-keyspace descriptor))

(def (cmd-create-invoice keyspace-id custid invid amt-str)
  (assert (stringp amt-str))
  (assert (nth-value 1 (gethash keyspace-id *descriptors*)))
  (assert (and (stringp custid) (stringp invid))) ;for compat with slack app
  (let ((now (get-universal-time)))
    (log-change
      `(create-invoice2 'cli ',now ',keyspace-id ',custid ',invid ',amt-str))
    (create-invoice2 'cli now keyspace-id custid invid amt-str)))

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
  (let ((ann (make-hash-table :test #'eql))) ;keyspace to custid to invs
    (for (a announcements)
      (let ((qc (car a)))
        (let ((ht (or (hash-get ann (qcust-keyspace qc))
                      (setf (gethash (qcust-keyspace qc) ann)
                            (make-hash-table :test #'equalp)))))
          (setf (gethash (qcust-custid qc) ht)
                (revappend (cdr a) (gethash (qcust-custid qc) ht '()))))))
    ann))

;; returns nested alist: keyspace to custid to invs
(def (announcements-ht-to-alist ann-ht)
  (let ((results '()))
    (maphash #'(lambda (keyspace ht)
                 (let ((alist '()))
                   (maphash #'(lambda (custid invs)
                                (push (cons custid invs) alist))
                            ht)
                   (push (cons keyspace alist) results)))
             ann-ht)
    results))
|#
