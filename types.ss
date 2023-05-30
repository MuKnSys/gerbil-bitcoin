(export #t)
(export (import: :clan/poo/mop) (import: :clan/poo/type) (import: :clan/poo/number))

(import
  (for-syntax :gerbil/gambit/exact :std/iter :std/stxutil :clan/syntax)
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact
  :gerbil/gambit/hash :gerbil/gambit/ports
  :std/assert :std/format :std/iter
  :std/misc/bytes :std/misc/completion :std/misc/hash :std/misc/list
  :std/sort
  :std/srfi/1 :std/srfi/13 :std/srfi/43
  :std/sugar
  :std/text/json
  :clan/base :clan/io :clan/json :clan/list
  :clan/maybe :clan/number :clan/syntax
  :clan/poo/object :clan/poo/io :clan/poo/rationaldict
  :clan/poo/mop
  :clan/poo/number
  :clan/poo/type
  :clan/poo/brace)

(define-type Hash Bytes32)
