(import
  :std/sugar
  ./chain ./scripts)

(def prevbest #f) ;optional, only to avoid needlessly filling up changelog

#|
;; Applies new chain activity to internal books. survives bitcoind unavail.
(def (cmd-poll-chain)
  (def (book-change best changes)
    (log-change `(book-chain-changes ',changes))
    (prog1
     (book-chain-changes changes)
     (set! prevbest best)))
  (def application
    (try (let ((best (getbestblockhash)))
           (unless (equal? best prevbest)
             (list best (compute-chain-changes best))))
         (catch (e) (display e (current-error-port)) #f))) ;TODO notify admin
  (when application (apply book-change application)))

(def (cmd-part-announcements )
  (when *announcements*
    (log-change-lax '(part-announcements))
    (part-announcements)))
|#