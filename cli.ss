;; the commands are a good place to strictly sanitise/typecheck all args.

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
  (when (cdr (assoc 'cli *parted-announcements*)) ; to not clutter changelog.
    (prog1
      (cli-process-announcements)
      ;log only after success, so you re-announce on crash recovery
      (log-change-lax '(cli-process-announcements)))))


(defvar *announcement-stream* *standard-output*)

(def (cli-process-announcements )
  (let ((entry (assoc 'cli *parted-announcements*)))
    (when entry
      (let ((*print-readably* t))
        (print (announcements-ht-to-alist (announcements-to-ht (cdr entry)))
               *announcement-stream*))
      (finish-output *announcement-stream*) ;must, as might log success soon
      (setf (cdr entry) '()))))

(def (announcements-to-ht announcements)
  (let ((ann (make-hash-table :test #'eql))) ;keyspace to custid to invs
    (dolist (a announcements ann)
      (let ((qc (car a)))
        (let ((ht (or (gethash (qcust-keyspace qc) ann)
                      (setf (gethash (qcust-keyspace qc) ann)
                            (make-hash-table :test #'equalp)))))
          (setf (gethash (qcust-custid qc) ht)
                (revappend (cdr a) (gethash (qcust-custid qc) ht '()))))))))

(def (announcements-ht-to-alist ann-ht)
  "returns nested alist: keyspace to custid to invs"
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
