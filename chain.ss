(export getblock3 chain-diff-vout-do get-block-at-depth)

(import
  :std/assert :std/misc/list :std/misc/list-builder :std/iter
  :clan/assert :clan/decimal :clan/memo :clan/number
  :clan/poo/object
  ./json-rpc)

(defstruct blk
  (hash
   height))

(defstruct (blk2 blk)
  (txs
   prevhash)) ;null for block 0

(defstruct tx2
  (id
   vouts))

(defstruct vout
  (index
   value
   address))

;; Memoizing version (TODO: only memoize a few of them)
(def getblock3 (memoizing getblock2))

(def (decode-tx wire-tx)
  (def id (.@ wire-tx txid))
  (assert! (pair? id))
  (make-tx2 id (filter-decode-vouts (.@ wire-tx vout))))

;; decodes and returns those vouts that have an address
(def (filter-decode-vouts wire-vouts)
  (with-list-builder (c)
    (for ((v wire-vouts))
      (def a (.@ v scriptPubKey addresses))
      (unless (void? a)
        (assert! (pair? a))
        (let ((index (.@ v n))
              (val (.@ v value)))
          (assert! (and (integer? index) (not (minus? index))))
          (assert! (not (minus? val)))
          (c (make-vout index val a)))))))

;;;; originally in chain_diff.txt

;; Returns (values pruned grafted), each a height sorted list of blocks and
;; lying above the common ancestor of the blocks OLD and NEW.
;; : (List blk) (List blk) <- blk blk
(def (chain-diff old new)
  (define-values (pruned old1) (higher-blocks new old))
  (define-values (grafted new1) (higher-blocks old1 new))
  (let loop ((o old1) (n new1) (p pruned) (g grafted))
    (if (equal? (blk-hash o) (blk-hash n))
      (values p g)
      (loop (prev-block o) (prev-block n) (cons o p) (cons n g)))))

(def (higher-blocks lower higher)
  (let loop ((y higher) (result '()))
    (if (<= (blk-height y) (blk-height lower))
      (values result y)
      (loop (prev-block y) (cons y result)))))

;;;; end extract from chain_diff.txt

(def (copy-blk blk)
  (make-blk (blk-hash blk) (blk-height blk)))

(def (prev-block b)
  ; assumption: TODO verify that blocks in alt chain have prev hash preserved.
  (copy-blk (getblock2 (blk2-prevhash (getblock2 (blk-hash b))))))

;;;;

(def (chain-diff-vout-do blkhash1 blkhash2 undo-fn redo-fn)
  "BLKHASH1 and BLKHASH2 identify two blocks in the block tree.
   This function computes the blocks between them that should be rolled back
   and rolled forward, then applies UNDO-FN to each transaction vout that
   should be rolled back and REDO-FN to each that should be rolled forward."
  (def b1 (copy-blk (getblock2 blkhash1)))
  (def b2 (copy-blk (getblock2 blkhash2)))
  (define-values (pruned grafted) (chain-diff b1 b2))
  (for (b (reverse pruned))
    (for (tx (reverse (blk2-txs (getblock2 (blk-hash b)))))
      (for (v (reverse (tx2-vouts tx)))
        (undo-fn (blk-hash b) (tx2-id tx) v))))
  (for (b grafted)
    (for (tx (blk2-txs (getblock2 (blk-hash b))))
      (for (v (tx2-vouts tx))
        (redo-fn (blk-hash b) (tx2-id tx) v)))))

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
      ((or (not (< 0 d)) (zero? (blk-height b))) b)))
