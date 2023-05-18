See comment at top of main.lisp
This shows how to bring up btcpay, though it fails to connect to the bitcoin
daemon.

* (asdf:load-asd "btcpay.asd")                 

T
* (asdf:load-system "btcpay")

T
* (ql:quickload "btcpay")
To load "btcpay":
  Load 1 ASDF system:
    btcpay
; Loading "btcpay"

("btcpay")

;;; drakma http client https://edicl.github.io/drakma/
* (ql:quickload :drakma)

* (drakma:http-request "http://lisp.org/")


* (pushnew :drakma-no-ssl *features*)

* (in-package :btcpay)

#<PACKAGE "BTCPAY">
* (test-pay)

(#<HASH-TABLE :TEST EQUALP :COUNT 0 {1003B947D3}>
 #<HASH-TABLE :TEST EQUALP :COUNT 0 {1003B94A43}>
 #<HASH-TABLE :TEST EQUALP :COUNT 1 {1003B948A3}>
 #<HASH-TABLE :TEST EQUALP :COUNT 1 {1003B94973}>)
* *announcements*

((C1 (I3 . 1)) (C1 (I2 . 10)) (C1 (I1 . 5)))

* *bitcoind-chain-url*

* (load "conf.lisp.template")

* *bitcoind-chain-url*
"http://localhost:18443"


* (initialize)

* (unless *lastblkhash* (cmd-poll-chain))
http-ka closing due to error: Condition USOCKET:CONNECTION-REFUSED-ERROR was signalled.
http-ka closing due to error: Condition USOCKET:CONNECTION-REFUSED-ERROR was signalled.
bitcoind connection failureNIL

* *bitcoind-chain-url*

"http://localhost:18443"
* *bitcoind-util-url*

"http://localhost:18443"
* *bitcoind-util-userpass*

("<user>" "<pass>")
* (defvar *bitcoind-util-userpass* '("donald" "meftah babkum es-sabar"))
(bc-bestblockhash)


--------------------------------------------------------------
;;; See also https://github.com/kylemanna/docker-bitcoind

;;;  Better, see https://en.bitcoin.it/wiki/Bitcoind


curl --user donald --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getbestblockhash", "params": []}' -H 'content-type: text/plain;' http://localhost:18443/
