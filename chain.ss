#;(export getblock3 chain-diff-vout-do get-block-at-depth)

(import
  :std/assert :std/misc/list :std/misc/list-builder :std/iter
  :clan/assert :clan/decimal :clan/memo :clan/number
  :clan/poo/object
  ./json-rpc)

;; Memoizing version (TODO: only memoize a few of them)
(def getblock3 (memoizing getblock2))

;;;; originally in chain_diff.txt

;; Returns (values pruned grafted), each a height sorted list of blocks and
;; lying above the common ancestor of the blocks OLD and NEW.
;; : (List blk) (List blk) <- blk blk
(def (chain-diff old new)
  (define-values (pruned old1) (higher-blocks new old))
  (define-values (grafted new1) (higher-blocks old1 new))
  (let loop ((o old1) (n new1) (p pruned) (g grafted))
    (if (equal? (.@ o hash) (.@ n hash))
      (values p g)
      (loop (prev-block o) (prev-block n) (cons o p) (cons n g)))))

(def (higher-blocks lower higher)
  (let loop ((y higher) (result '()))
    (if (<= (.@ y height) (.@ lower height))
      (values result y)
      (loop (prev-block y) (cons y result)))))

;;;; end extract from chain_diff.txt

(def (prev-block b)
  ;; assumption: TODO verify that blocks in alt chain have prev hash preserved.
  (getblock2 (.@ b previousblockhash)))

;;;;

(def (chain-diff-vout-do blkhash1 blkhash2 undo-fn redo-fn)
  "BLKHASH1 and BLKHASH2 identify two blocks in the block tree.
   This function computes the blocks between them that should be rolled back
   and rolled forward, then applies UNDO-FN to each transaction vout that
   should be rolled back and REDO-FN to each that should be rolled forward."
  (def b1 (getblock2 blkhash1))
  (def b2 (getblock2 blkhash2))
  (define-values (pruned grafted) (chain-diff b1 b2))
  (for (b (reverse pruned))
    (for (tx (reverse (.@ (getblock2 (.@ b hash)) tx)))
      (for (v (reverse (.@ tx vout)))
        (undo-fn (.@ b hash) (.@ tx txid) v))))
  (for (b grafted)
    (for (tx (.@ (getblock2 (.@ b hash)) tx))
      (for (v (.@ tx vout))
        (redo-fn (.@ b hash) (.@ tx txid) v)))))

;;;;

(def (get-block-at-depth blockhash depth)
  "DEPTH 0 specifies the block at BLOCKHASH, 1 specifies its parent, etc.
   This procedure walks down the block tree.
   Height arithmetic would be unreliable as the tree may change while doing it.
   DEPTH should be a whole number no larger than BLOCKHASH's height.
   If it is larger, then the deepest block (i.e. at height 0) is returned.
   (get-block-at-depth (getbestblockhash) (1- desired-confirmations)) ."
  (let loop ((b (getblock2 blockhash))
             (d depth))
    (if (and (plus? d) (plus? (.@ b height)))
      (loop (getblock2 (.@ b previousblockhash)) (1- d))
      b)))
