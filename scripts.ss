#;(export
  compute-chain-changes book-chain-changes
  create-invoice2 process-announcements register-keyspace)

(import
  :gerbil/gambit/ports
  :std/assert :std/misc/hash :std/misc/list :std/misc/list-builder :std/misc/number
  :std/srfi/13 :std/sugar
  :clan/base :clan/basic-parsers :clan/decimal
  ./keys)

;;;; create invoice ;;;;

;; keyspace to invoice counter
(def invoice-counters (hash))

(def (next-invoice-counter keyspace)
  (increment! (hash-ref invoice-counters keyspace -1)))

(defstruct invoice-reference
  (intid
   extid))

(def (create-invoice time keyspace customer-id invoice-id amount-str)
  (def amount (decimal<-string amount-str sign-allowed?: #f))
  (when (and amount (hash-key? descriptor<-keyspace keyspace))
    (let* ((qc (make-qcust keyspace customer-id))
           (da (or (address<-customer qc) (try-add-cust3 qc time))))
      (when da
        '(generate-charge qc invoice-id amount) ;; TODO report event
        (cons da amount)))))

(def addr-custs2 (hash))

(def (try-add-cust3 qcust time)
  (def da (try-add-customer qcust time))
  (when da
    ;;we allow multiple keyspaces/descriptors that derive same keys. tell all.
    (let (daa (derived-address-address da))
      (push! qcust (hash-get addr-custs2 daa)))
    da))

#|

#|
Definition: Two keyspaces overlap if there is some address that both can derive.
A set of non-overlapping keyspaces plus the chain form a balanced accounting
system.
So a single keyspace plus the chain also forms a balanced system.
A set of keyspaces where some overlap, plus the chain, are not a balanced
system.
|#

(def dedup-invoice-delay-in-seconds (* 60 60 24 3))
(def dedup-create-invoice
  (dedup-calls dedup-invoice
               (lambda a (cons (apply create-invoice a) a))
               dedup-invoice-delay-in-seconds))

(def (create-invoice2 tag time keyspace customer invoice amount-str)
  (def callkey (cons keyspace inv))
  (def matchargs (cons cust amount-str))
  (define-values (prv hit2) (dedup-lookup dedup-invoice callkey))
  (def (invoice-response1 create-result tinv)
    (if create-result
      ['invoice-address
       (object->string (invoice-reference-intid (invoice-body tinv)))
       (printable-der-addr (car create-result)) ...
       (print-btc (cdr create-result))]
      '(no-answer)))
  (if (not hit2)
    (let ((inv (make-tagged-invoice
                tag
                (make-invoice-reference (next-inv-ctr keyspace) inv))))
      (invoice-response1
       (car (dedup-create-invoice time callkey
                                  time keyspace cust inv amt-str))
       inv))
    (with ((list pres ptm pks pcust pinv pamtstr) (car prv))
      (if (and (equal? cust pcust) (equal? amt-str pamtstr))
        (invoice-response1 pres pinv)
        '(mismatching-duplicate-request)))))

(def (printable-derived-address da)
  [(object->string (derived-address-index da))
   (derived-address-address da)
   (object->string (derived-address-birthtime da))])

;;;; monitor chain ;;;;

;; must be positive. tip of chain has one conf.
(def *confirmations* 1)
; we model confirmations as a lag that we operate at.
; all blocks at height zero are considered to have been born with infinite
;   confirmations. i.e. if this parameter is larger than chain length, you will
;   still announce block zero.
; one way to notify at e.g. two different confirmation levels is to
;   have two sets of (books (pay.lisp) + lastblkhash)

(def *lastblkhash* nil)
; when replaying events, what if bitcoind no longer has some blocks?
; e.g. those from abandoned forks or ancient blocks when in pruning mode.
; you should ignore their absence and carry on. in fact skipping all
; intermediate blocks before current best should also give you same resulting
; account balances and notifications. except where you'd sent out a "paid"
; notification but then found a rollback. this time you might not make that
; spurious notification at all and might make a fresh one in place of it in the
; future.
; you can cache blocks as json files named by hash
; [solved: now I log the computed delta so don't fetch from bitcoind on replay.
;          this also makes reasoning about overall application easier, because
;          now completely deterministic.]

;; does not change application state, but fetches blockchain data for logging
;; into the changelog
(def (compute-chain-changes bestblockhash)
  (let ((h (blk2-hash (get-block-at-depth bestblockhash (1- *confirmations*)))))
    (cons h (fetch-payments1 h))))

;; changes application state without needing access to the blockchain
(def (book-chain-changes changes)
  (book-payments (cdr changes))
  (setf *lastblkhash* (car changes)))

;; Returns a list of chain-payments to be applied. It's correct for this
;; procedure to be called at initialisation time when no addresses are yet
;; being observed.
(def (fetch-payments1 hash)
  (if *lastblkhash*
    (fetch-payments *lastblkhash* hash addr-custs2)
    (assert! (hash-empty? addr-custs2))))

(defstruct chain-payment
  (tag ; - to rollback, + to roll forward
   to-address
   vout-id ; (blkhash txid vout-index)
   amt))

;; Scans chain between OLDBLKHASH and NEWBLKHASH for information related (only)
;; to ADDRESSES. Returns a list of chain-payments. May raise RPC error.
(def (fetch-payments oldblkhash newblkhash addresses)
  (with-list-builder (c)
    (def (cb tag)
      (lambda (blkhash txid vout)
        (when (hash-key? addresses (vout-address vout))
          (c (make-chain-payment
              tag: tag
              to-address: (vout-address vout)
              vout-id: (list blkhash txid (vout-index vout))
              amt: (vout-value vout))))))
    (chain-diff-vout-do oldblkhash newblkhash (cb '-) (cb '+))))

;; You can go up and down between two given blocks any number of times, which
;; means that the vout-id alone is not unique in our books. So we combine it
;; with this counter's value.
(def *payment-id-ctr* -1)

(def (book-payments payments)
  (for-each (lambda (p)
              (let ((fn (case (chain-payment-tag p)
                          ((-) 'generate-priority-charge)
                          ((+) 'receive-payment)))
                    (vout-id (chain-payment-vout-id p))
                    (amt (chain-payment-amt p)))
                (for-each (lambda (cust)
                            (fn cust
                                ['chain (pre-increment! *payment-id-ctr*) vout-id ...]
                                amt))
                          (hash-get addr-custs2 (chain-payment-to-address p)))))
            payments))

;;;; parting announced invoices by tag ;;;;

; there's a user-level invid. each application puts its own tag on it.
; at the lowest level, pay.lisp puts its own tag on that.
; the slack application ensures that the user-level invid is unique within
; a keyspace, but the cli application currently doesn't.
; our software doesn't depend on uniqueness.
; the inv is an appropriate place for correlation info, whether an internal ID,
; an external ID, metadata needed for routing, or any combination.
; *announcements* itself has the right order, so may not be the right place to
; split announcements up by application. that's why for now the splitting up,
; and abandoning of overall order, is each application's choice.
; the slack application does preserve order of slack announcements, and within
; a channel, the announcements for that channel. no harm abandoning those
; ordering guarantees.
; we don't yet need messages ordered, but might if we add features, so we make
; an effort to not weaken our ordering guarantees lightly.

(def (make-tagged-invoice tag invoice) (cons tag invoice))
(def (invoice-tag inv) (car inv))
(def (invoice-body inv) (cdr inv))
(def (invoice-tag-matches invoice tag) (equal? (car invoice) tag))

;; table from tag to its announcements (from which tags are removed)
(def *parted-announcements* (hash))

(def (part-announcements )
  "*ANNOUNCEMENTS* has all invoices regardless of tag. This procedure parts
   them by tag. Data is moved from *ANNOUNCEMENTS* to *PARTED-ANNOUNCEMENTS*,
   which is a table keyed by tag and whose values are each of the same shape as
   the ANNOUNCEMENTS table itself. The only difference is that invoices in
   *PARTED-ANNOUNCEMENTS* have had their tags removed. Also, they have INV-ANN
   structs instead of conses."
  (def partitioned (hash))
  (for (ann *announcements*)
      (with ([qc . tagged-invs] ann)
        (let ((row-invs (hash)))
          (for (ti1 tagged-invs)
            (destructuring-bind (ti . amt) ti1
              (let ((ref (inv-body ti)))
                 (hash-ensure-modify!
                  row-invs (inv-tag ti) list
                  (cut cons (make-inv-ann :intid (inv-ref-intid ref)
                                    :extid (inv-ref-extid ref)
                                    :amt amt)
                            <>)))))
          (for ((values k v) row-invs)
            (hash-ensure-modify!
             partitioned k list (cons qc (reverse v)))))
    (for (p partitioned)
      (def entry (or (assoc (car p) *parted-announcements*)
                     (car (push (list (car p)) *parted-announcements*))))
      (set! (cdr entry) (append-reverse (cdr p) (cdr entry))))))
  (set! *announcements* '()))

(defstruct inv-ann
  intid extid amt)

(def (printable-inv-ann ia custid)
  (list (princ-to-string (inv-ann-intid ia))
        ;the next three are for convenience only. user can infer from intid.
        (print-btc (inv-ann-amt ia))
        custid
        (inv-ann-extid ia)))

#|
The outside of the application helps a user not submit the same
invoice ID/amount request twice within the same couple of days. Such a duplicate
request produces the memoized answer. The user must know that the answer is a
duplicate and that we consider the second invoice to be the same as the first
one. We promise no more to the user.
However, the inside of the application assumes nothing about the invoice ID.
It is capable of handling two with the same ID. And if the second is requested
many days later then the outer layer's short-term cache won't deflect the second
request anyway. Our system will even correctly produce separate "invoice paid"
announcements for each of the two similar invoices. But the user has no way to
know. The user has been told that two announcements of the same invoice ID refer
to just one invoice. If he's lucky, he can differentiate them when their amounts
are different.
Now think about the cryptosystem where an announcement is a user's claim on us.
We are still OK because two announcements with the same contents
(i.e. invoice ID/amount) turn into just one claim on us. If we'd like the user
to properly claim twice, we should augment the user's invoice ID with a
unique internally generated one. We then always refer to invoices by this
augmented ID. Then the user can differentiate two announcements that are for
conceptually different invoices that have the same external ID/amount.
For now, it's fine to continue telling the user that it's in their interest to
provide unique external invoice ID's.
Update: augmentation done.
|#


;;;; keyspace registration ;;;;

(defvar *keyspace-counter* -1)

(def (register-keyspace descriptor)
  "returns the new registration's keyspace ID if successful"
  (when (descriptor-valid? descriptor)
    (let ((keyspace-id (incf *keyspace-counter*)))
      (setf (gethash keyspace-id *descriptors*) descriptor)
      keyspace-id)))

(def (descriptor-valid? descriptor)
  (and (string? descriptor)
       (every (lambda (c) (or (ascii-alphanumeric? c) (string-index c "()[]'/*#")))
              descriptor)
       (string-prefix? "wpkh" descriptor)
       (ignore-errors
        (let (info (bc-descinfo descriptor))
          (and (equal? (.@ info descriptor) descriptor)
               (equal? (.@ info isrange) #t)
               (derive-address descriptor 0))))))
|#
