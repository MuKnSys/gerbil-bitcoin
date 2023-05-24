(export)

(import
  )

#|
;;;; exports: initialize
;;;; after starting Lisp and loading the program,
;;;;   call initialize, then you may pop in and out of the main-loop at will.
;;;; snapshots aren't implemented yet, only a change log. it'll do.

(def (initialize)
  (complete-partial-snapshot)
  (when (probe-file (snapshot-file))
    (load-snapshot (snapshot-file)))
  (when (probe-file (changes-file))
    (replay-changes))
  (open-changes-file-for-append))

(def (replay-changes)
  ;TODO if an entry is partial, assert it's final one and have user fix/restart.
  ;last announcement may not have been logged before crash. we re-announce.
  ;  user must be prepared for this possible duplicate announcement.
  (let ((*announcement-stream* *dev-null*)
        (*read-eval* nil))
    (load (changes-file))))

(def (load-snapshot file)
  ;TODO
 )

;;;; snapshot.dat has a snapshot.
;;;; changes.dat has changes since then.
;;;; neither is required to exist.
;;;; taking a snapshot archives these two files to ones with timestamped names
;;;;   and creates a new snapshot.dat.
;;;; this code is written so that,
;;;;   if the process crashes, it will restore state on restart.
;;;;   even if restart crashes, next restart will resume restoration.
;;;; the archived and current changes files together form a complete log.

;;;;

; if this signals an error, you must fix the problem and try again.
; don't continue using the process without having run this to completion.
(def (snapshot)
  (close-changes-file) ;no-op if already closed. must close or signal error.
  (snapshot-to-stg)
  (swap-in-stg)
  (open-changes-file-for-append))

; be careful with dirs in pathnames. must support rename-file.
; same goes for #'changes-file
(def (snapshot-file) "snapshot40239.dat")
(def (snapshot-stgfile) "snapshot40239.stg")
(def (snapshot-tmpfile) "snapshot40239.tmp")
(def (archive-date-file) "archive40239.dat")
(def (archive-date-tmpfile) "archive40239.tmp")
(def (snapshot-filename ts) (format nil "snapshot40239-~A.dat" ts))
(def (changes-filename ts) (format nil "changes40239-~A.dat" ts))

(def (complete-partial-snapshot)
  (when (probe-file (snapshot-stgfile))
    (swap-in-stg)))

(def (swap-in-stg)
  (archive-old-snapshot)
  (rename-file (snapshot-stgfile) (snapshot-file)))

(def (archive-old-snapshot)
  (when (some #'probe-file (list (snapshot-file) (changes-file)))
    (archive-old-snapshot1))
  (when (probe-file (archive-date-file))
    (delete-file (archive-date-file))))

(def (archive-old-snapshot1)
  (let ((ts2
         (cond ((probe-file (archive-date-file))
                (slurp-file (archive-date-file)))
               (t
                (let ((ts (princ-to-string (get-universal-time)))) ;max 1 snap/s
                  (assert
                    (notany #'probe-file (list (snapshot-filename ts)
                                               (changes-filename ts))))
                  (with-open-file (out (archive-date-tmpfile)
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
                    (write-string ts))
                  (rename-file (archive-date-tmpfile) (archive-date-file))
                  ts)))))
    (when (probe-file (snapshot-file))
      (assert (null (probe-file (snapshot-filename ts2))))
      (rename-file (snapshot-file) (snapshot-filename ts2)))
    (when (probe-file (changes-file))
      (assert (null (probe-file (changes-filename ts2))))
      (rename-file (changes-file) (changes-filename ts2)))))

(def (snapshot-to-stg)
  (when (probe-file (snapshot-tmpfile)) (delete-file (snapshot-tmpfile)))
  (when (probe-file (snapshot-stgfile)) (delete-file (snapshot-stgfile)))
  (snapshot-to-tmp)
  (rename-file (snapshot-tmpfile) (snapshot-stgfile)))

(def (snapshot-to-tmp)
  ;TODO
  ;can even snapshot whole lisp image for now (in impls where that doesn't quit)
  (void))
|#
