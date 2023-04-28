(in-package "BTCPAY")

;; there is support for two bitcoind's. one for chain information, which we
;; need trust less, and another for utility functions such as address
;; derivation, which must be more trustworthy.
;; the chain one's going down won't crash our program.
;; the utility one doesn't need any blocks downloaded or stored. e.g.,
;;     bitcoind -connect 127.0.0.1:62000
;;   or something, where nothing's listening on port 62000, will prevent it
;;   from connecting to real peers and downloading the chain.
(def *bitcoind-chain-url* nil)
(def *bitcoind-chain-userpass* nil)
(def *bitcoind-chain-conn* (make-ka-conn))
(def *bitcoind-util-url* nil)
(def *bitcoind-util-userpass* nil)
(def *bitcoind-util-conn* (make-ka-conn))

(def (set-bitcoind-chain-url url)
  (print `(set-bitcoind-chain-url ,url))
  (close-ka-conn *bitcoind-chain-conn*)
  (setf *bitcoind-chain-url* url
        *bitcoind-chain-conn* (make-ka-conn)))

(def (set-bitcoind-util-url url)
  (close-ka-conn *bitcoind-util-conn*)
  (setf *bitcoind-util-url* url
        *bitcoind-util-conn* (make-ka-conn)))

#||
(define-condition proc-error error)
    ((http-code :initarg :http-code
                :initform nil
                :reader proc-error-http-code)
     (http-reason :initarg :http-reason
                  :initform nil
                  :reader proc-error-http-reason)
     (rpc-error :initarg :rpc-error
                :initform nil
                :reader proc-error-rpc-error))
  (:report (lambda (condition stream)
             (format stream
                     "JSON-RPC failed (HTTP ~A ~A) (RPC ~A)"
                     (proc-error-http-code condition)
                     (proc-error-http-reason condition)
                     (proc-error-rpc-error condition)))))
||#

(def (bitcoind-rpc conn url userpass meth . params)
  "Raises an error if TCP or HTTP error[*] or malformed JSON response.
   Otherwise raises a PROC-ERROR if JSON-RPC response has an error member.
   Otherwise returns result member's json decoded value.
   Separating error types lets callers use bitcoind for validation.
   [*]: except 500, which is also treated as a PROC-ERROR. bad protocol."
  (let ((resp (http-ka-request
                conn url :basic-authorization userpass
                :method :post :force-binary t
                :content-type "application/json"
                :content
                  (encode-json-alist-to-string
                    (list '("id" . "") '("jsonrpc" . "1.0")
                          (cons "method" meth) (cons "params" params))))))
    (cond ((null (cdr resp)) (error "bitcoind connection failure"))
          ((equal (cadr resp) 500) ;bitcoind should complain with 200. doesn't.
           (error 'proc-error :http-code (cadr resp) :http-reason (nth 6 resp)))
          ((not (equal (cadr resp) 200))
           (error "http error ~A ~A" (cadr resp) (nth 6 resp)))
          (else
           (let ((resp-obj
                   (decode-json-from-string
                     (map 'string #'code-char (car resp))))) ; assumption: ASCII
             (let ((rpc-error (assoc :error resp-obj))
                   (rpc-result (assoc :result resp-obj)))
               (cond ((cdr rpc-error) ;not used. bitcoind stupidly sends 500.
                      (error 'proc-error :rpc-error (cdr rpc-error)))
                     ((null rpc-result) (error "malformed rpc response"))
                     (else (cdr rpc-result)))))))))

(def (bitcoind-chain-rpc meth . params)
  (apply #'bitcoind-rpc *bitcoind-chain-conn* *bitcoind-chain-url*
                        *bitcoind-chain-userpass* meth params))

(def (bitcoind-util-rpc meth . params)
  (apply #'bitcoind-rpc *bitcoind-util-conn* *bitcoind-util-url*
                        *bitcoind-util-userpass* meth params))

;;;; See https://developer.bitcoin.org/reference/rpc/index.html

;;;; Blockchain RPCs

;;; See https://developer.bitcoin.org/reference/rpc/getbestblockhash.html
;;; getbestblockhash
;;; bitcoin-cli getbestblockhash
;;; curl --user myusername --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getbestblockhash", "params": []}' -H 'content-type: text/plain;' http://127.0.0.1:8332/
;;; Returns the hash of the best (tip) block
;;; in the most-work fully-validated chain.
(def (bc-bestblockhash)
  (bitcoind-chain-rpc "getbestblockhash"))

;;; See https://developer.bitcoin.org/reference/rpc/getblock.html
;;; getblock "blockhash" ( verbosity )
;;; bitcoin-cli getblock "00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09"
;;; curl --user myusername --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getblock", "params": ["00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09"]}' -H 'content-type: text/plain;' http://127.0.0.1:8332/
;;; If verbosity is 0, returns a string that is serialized,
;;; hex-encoded data for block ‘hash’.
;;; If verbosity is 1, returns an Object with information about block ‘hash’.
;;; If verbosity is 2, returns an Object with information about block ‘hash’
;;; and information about each transaction.
(def (bc-block-verbose-2 hash)
  (let ((*real-handler* #'values)) ; keeps cl-json reals (not ints) as strings
    (bitcoind-chain-rpc "getblock" hash 2)))

;;;; Util RPCs

;;; See https://developer.bitcoin.org/reference/rpc/getdescriptorinfo.html
;;; getdescriptorinfo "descriptor"
;;; bitcoin-cli getdescriptorinfo "wpkh([d34db33f/84h/0h/0h]0279be667ef9dcbbac55a06295Ce870b07029Bfcdb2dce28d959f2815b16f81798)"
;;; Returns
;;; {                                   (json object)
;;;   "descriptor" : "str",             (string) The descriptor in canonical form, without private keys
;;;   "checksum" : "str",               (string) The checksum for the input descriptor
;;;   "isrange" : true|false,           (boolean) Whether the descriptor is ranged
;;;   "issolvable" : true|false,        (boolean) Whether the descriptor is solvable
;;;   "hasprivatekeys" : true|false     (boolean) Whether the input descriptor contained at least one private key
;;; }
;;; Analyses a descriptor.
(def (bc-descinfo prevalidated-desc)
  (bitcoind-util-rpc "getdescriptorinfo" prevalidated-desc))

;;; deriveaddresses "descriptor" ( range )
;;; A descriptor wallet is one which stores output descriptors
;;; and uses them to create addresses and sign transactions. 
;;; Derives one or more addresses corresponding to an output descriptor.
;;; Examples of output descriptors are:
;;; pkh(<pubkey>) P2PKH outputs for the given pubkey wpkh(<pubkey>)
;;; Native segwit P2PKH outputs for the given pubkey
;;; sh(multi(<n>,<pubkey>,<pubkey>,…)) P2SH-multisig outputs
;;; for the given threshold and pubkeys raw(<hex script>)
;;; Outputs whose scriptPubKey equals the specified hex scripts
;;; In the above, <pubkey> either refers to a fixed public key
;;; in hexadecimal notation, or to an xpub/xprv optionally
;;; followed by one or more path elements separated by “/”,
;;; where “h” represents a hardened child key.
;;; For more information on output descriptors, see the documentation
;;; in the doc/descriptors.md file.
(def (bc-deriveaddress1 descriptor index)
  (bitcoind-util-rpc "deriveaddresses" descriptor (list index index)))

;;;; Wallet RPCs

;;; See https://developer.bitcoin.org/reference/rpc/signmessage.html
;;; signmessage "address" "message"
;;; Sign a message with the private key of an address.
;;; Requires wallet passphrase to be set with walletpassphrase call
;;; if wallet is encrypted.
(def (bc-sign address message)
  (bitcoind-util-rpc "signmessage" address message))
