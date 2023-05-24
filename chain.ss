(export #t)
;;;; exports: getbestblockhash getblock2 chain-diff-vout-do get-block-at-depth

(import
  :clan/assert
  ./bitcoinc)

; signals an error if it doesn't succeed
(def (getbestblockhash)
  (let ((result (bc-bestblockhash)))
    (assert-equal! (length result) 64)
    result))

;;;;

(defstruct blk2
  (hash
   height
   txs
   prevhash)) ;null for block 0

(defstruct tx2
  (id
   vouts))

(defstruct vout
  (index
   value
   address))

#|
; signals an error on failure
(def (getblock hash)
  "fetches a block, decodes it and returns a blk2"
  (let ((b (bc-block-verbose-2 hash)))
    (let ((hash2 (cdr (assoc :hash b)))
          (height2 (cdr (assoc :height b)))
          (txs (cdr (assoc :tx b)))
          (prev (cdr (assoc :previousblockhash b))))
      (assert (= (length hash2) 64))
      (assert (equalp hash hash2))
      (assert (not (minusp height2)))
      (assert (not (null txs)))
      (assert (or (and (zerop height2) (null prev))
                  (= (length prev) 64)))
      (make-blk2 :hash hash2
                 :height height2
                 :txs (mapcar #'decode-tx txs)
                 :prevhash prev))))

(def (decode-tx wire-tx)
  (let ((id (cdr (assoc :txid wire-tx))))
    (assert (plusp (length id)))
    (make-tx2 :id id :vouts (filter-decode-vouts (cdr (assoc :vout wire-tx))))))

(def (filter-decode-vouts wire-vouts)
  "decodes and returns those vouts that have an address"
  (let ((results '()))
    (dolist (v wire-vouts (nreverse results))
      (let ((a (cdr (assoc :address (cdr (assoc :script-pub-key v))))))
        (when a
          (assert (plusp (length a)))
          (let ((index (cdr (assoc :n v)))
                (val-string (cdr (assoc :value v))))
            (assert (and (integerp index) (not (minusp index))))
            (assert (plusp (length val-string)))
            (let ((val (parse-non-neg-btc val-string)))
              (assert val)
              (push (make-vout :index index :value val :address a)
                    results))))))))

(def getblock2 (bounded-memoize #'getblock 20))

;;;;

;;;; originally in chain_diff.txt

(defstruct blk
  (hash
   height))

(def (chain-diff old new)
  "Returns (pruned grafted), each a height sorted list of blocks and
  lying above the common ancestor of the blocks OLD and NEW."
  (with [pruned old1] (higher-blocks new old)
    (with [grafted new1] (higher-blocks old1 new)
      (do ((o old1    (prev-block o))
           (n new1    (prev-block n))
           (p pruned  (cons o p))
           (g grafted (cons n g)))
          ((equalp (blk-hash o) (blk-hash n)) (list p g))))))

(def (higher-blocks lower higher)
  (do ((y higher (prev-block y))
       (result '() (cons y result)))
      ((<= (blk-height y) (blk-height lower)) (list result y))))

;;;; end extract from chain_diff.txt


(def (make-blk1 blk2)
  (make-blk :hash (blk2-hash blk2) :height (blk2-height blk2)))

(def (prev-block b)
  ; assumption: TODO verify that blocks in alt chain have prev hash preserved.
  (make-blk1 (getblock2 (blk2-prevhash (getblock2 (blk-hash b))))))

;;;;

(def (chain-diff-vout-do blkhash1 blkhash2 undo-fn redo-fn)
  "BLKHASH1 and BLKHASH2 identify two blocks in the block tree.
   This function computes the blocks between them that should be rolled back
   and rolled forward, then applies UNDO-FN to each transaction vout that
   should be rolled back and REDO-FN to each that should be rolled forward."
  (let ((b1 (make-blk1 (getblock2 blkhash1)))
        (b2 (make-blk1 (getblock2 blkhash2))))
    (with [pruned grafted] (chain-diff b1 b2)
      (dolist (b (nreverse pruned))
        (dolist (tx (reverse (blk2-txs (getblock2 (blk-hash b)))))
          (mapc #'(lambda (v) (funcall undo-fn (blk-hash b) (tx2-id tx) v))
                (reverse (tx2-vouts tx)))))
      (dolist (b grafted)
        (dolist (tx (blk2-txs (getblock2 (blk-hash b))))
          (mapc #'(lambda (v) (funcall redo-fn (blk-hash b) (tx2-id tx) v))
                (tx2-vouts tx)))))))

;;;;

(def (get-block-at-depth blockhash depth)
  "DEPTH 0 specifies the block at BLOCKHASH, 1 specifies its parent, etc.
   This procedure walks down the block tree.
   Height arithmetic would be unreliable as the tree may change while doing it.
   DEPTH should be a whole number no larger than BLOCKHASH's height.
   If it is larger, then the deepest block (i.e. at height 0) is returned.
   (get-block-at-depth (getbestblockhash) (1- desired-confirmations)) ."
  (do ((b (getblock2 blockhash) (getblock2 (blk2-prevhash b)))
       (d depth (1- d)))
      ((or (not (< 0 d)) (zero? (blk2-height b))) b)))
|#
