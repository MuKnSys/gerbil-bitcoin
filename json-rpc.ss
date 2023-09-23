;; See this reference documentation (that explicitly disclaims being a specification):
;; https://developer.bitcoin.org/reference/rpc/
;; TODO: build an interface for each and every function in that reference document.

(export #t)

(import
  (for-syntax :std/format)
  :std/misc/list :std/misc/ports :std/srfi/13
  :clan/basic-parsers :clan/config :clan/logger :clan/path :clan/syntax
  :clan/net/json-rpc
  :clan/poo/object :clan/poo/mop :clan/poo/type
  ./types)

;; Logging support for bitcoin
;; TODO: change clan/logger to make configurable whether logging is to files or something else.
(def btc-log (json-logger "bitcoin"))

;; We support for two bitcoind:
;; - One bitcoind watches the blockchain;
;;   we need to trust it with data availability, but not with keys.
;;   If it crashes, that should not interrupt the key handling functionality,
;;   and watch will resume forthwith after it's restarted.
;; - One bitcoind handles key derivation;
;;   we need to trust it to give us good answers about keys,
;;   but it need not be connected to the network at all.
;;   It could be run trying to connect to a closed port 6666 or something like this:
;;     bitcoind -connect 127.0.0.1:6666
;; The two can also be the same, that must then be connected to the network, yet
;; secure enough to not fake public key derivation or leak private key information.

(defstruct BitcoindConnection
  ;; TODO: in the future, support keepalive and retrying
  ;; TODO: add a mutex for access to a connection, not to overload it and get timeouts.
  (url ;; URL ;;
   auth) ;; : Auth ;; auth argument for std/net/request, e.g. #f or [basic: "myusername" "mypassword"]
  transparent: #t)

(def (bitcoind-connection url (auth #f))
  (BitcoindConnection url auth))

;; TODO: have one connection per endpoint? Per set of RPCs? (Blockchain, Control, Generating, Mining, Network, Rawtransactions, Util, Wallet) Looked from as many variables? From a single structure?

;; : (OrFalse BitcoindConnection)
(def bitcoind-chain #f)

;; : (OrFalse BitcoindConnection)
(def bitcoind-keys #f)


;; TODO: use clan/net/simple-http-client ? clan/net/json-rpc ?
;; Raises an error if TCP or HTTP error[*] or malformed JSON response.
;; Otherwise raises a PROC-ERROR if JSON-RPC response has an error member.
;; Otherwise returns result member's json decoded value.
;; Separating error types lets callers use bitcoind for validation.
;; [*]: except 500, which is also treated as a PROC-ERROR. bad protocol.
(def (bitcoind-rpc connection
                   log: (log btc-log) timeout: (timeout #f)
                   method result-decoder param-encoder params)
  (with ((BitcoindConnection url auth) connection)
    (json-rpc url method params
              result-decoder: result-decoder
              param-encoder: param-encoder
              timeout: timeout
              log: log
              auth: auth)))

(defsyntax (define-bitcoin-api stx)
  (syntax-case stx (<-)
    ((_ connection method result-type <- argument-type ...)
     (let*-values (((method-name method-formals args-vector)
                    (syntax-case #'method ()
                      ((name . formals)
                       (values #'name #'formals (call<-formals #'(vector) #'formals)))
                      (name
                       (let* ((n (length (syntax->datum #'(argument-type ...))))
                              (vars (formals<-nat n)))
                         (values #'name vars (cons 'vector vars))))))
                   ((method-string) (symbol->string (syntax->datum #'method-name)))
                   ((connection-id) (datum->syntax (stx-car stx)
                                      (string->symbol (format "bitcoind-~a"
                                                              (syntax->datum #'connection))))))
       (with-syntax (((formals ...) method-formals)
                     (args-vector args-vector)
                     (method-string method-string)
                     (method-name method-name)
                     (connection-id connection-id))
         #'(begin
             (def params-type (Tuple argument-type ...))
             (def (method-name timeout: (timeout #f) log: (log btc-log) formals ...)
               (bitcoind-rpc connection-id method-string
                             (.@ result-type .<-json)
                             (.@ params-type .json<-) args-vector
                             timeout: timeout log: log))))))))

(def (ensure-bitcoin-connection connection (keys-connection connection))
  (set! bitcoind-chain connection)
  (set! bitcoind-keys keys-connection))

;; Separate a string in two, according to the first occurence of the separator character,
;; typically #\: or #\=
;; : (OrFalse (List String String)) <- String Char
(def (string-separate string separator)
  (def i (string-index string separator))
  (and i [(string-take string i) (string-drop string (1+ i))]))

(def (get-regtest-cookie)
  (string-separate (read-file-string (subpath (user-home) ".bitcoin/regtest/.cookie"))
                   #\:))

(def (ensure-test-bitcoin-connection)
  (ensure-bitcoin-connection
   (bitcoind-connection "http://127.0.0.1:18443"
                        [basic: (get-regtest-cookie)...])))

;;;; Blockchain RPCs

;;; See https://developer.bitcoin.org/reference/rpc/getbestblockhash.html
;;; getbestblockhash
;;; bitcoin-cli getbestblockhash
;;; curl --user myusername --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getbestblockhash", "params": []}' -H 'content-type: text/plain;' http://127.0.0.1:8332/
;;; Returns the hash of the best (tip) block
;;; in the most-work fully-validated chain.
(define-bitcoin-api chain getbestblockhash
  Hash <-)

;;; See https://developer.bitcoin.org/reference/rpc/getblock.html
;;; getblock "blockhash" ( verbosity )
;;; bitcoin-cli getblock "00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09"
;;; curl --user myusername --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getblock", "params": ["00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09"]}' -H 'content-type: text/plain;' http://127.0.0.1:8332/
;;; If verbosity is 0, returns a string that is serialized,
;;; hex-encoded data for block ‘hash’.
;;; If verbosity is 1, returns an Object with information about block ‘hash’.
;;; If verbosity is 2, returns an Object with information about block ‘hash’
;;; and information about each transaction.
(define-bitcoin-api chain getblock
  Json <- Hash Integer)

;; https://developer.bitcoin.org/reference/rpc/getrawtransaction.html
(define-type TxInfo ;; for verbose output of getrawtransaction or verbose 2 output of getblock
  (Record
   in_active_chain: [Bool] ;; Whether specified block is in the active chain or not (only present with explicit "blockhash" argument)
   hex: [Bytes] ;; The serialized, hex-encoded data for 'txid'
   txid: [Bytes] ;; The transaction id (same as provided for getrawtransaction)
   hash: [Hash] ;; The transaction hash (differs from txid for witness transactions)
   size: [JsInt] ;; The serialized transaction size
   vsize: [JsInt] ;; The virtual transaction size (differs from size for witness transactions)
   weight: [JsInt] ;; The transaction's weight (between vsize*4-3 and vsize*4)
   version: [JsInt] ;; The version
   locktime: [JsInt] ;; The lock time
   vin: [(List VInInfo)]
   vout: [(List VOutInfo)]))

(define-type VInInfo
  (Record
   txid: [Bytes] ;; The transaction id
   vout: [JsInt] ;; The output number
   scriptSig: [ScriptSig] ;; The script
   sequence: [JsInt] ;; The script sequence number
   txinwitness: [(List Bytes)])) ;; hex-encoded witness data (if any)

(define-type ScriptSig
  (Record
   asm: [String]
   hex: [Bytes]))

(define-type VOutInfo
  (Record
   value: [Float] ;; The value in BTC (not satoshi)
   n: [JsInt] ;; index
   scriptPubKey: [ScriptPubKey]
   blockhash: [Hash] ;; the block hash
   confirmations: [JsInt] ;; The confirmations
   blocktime: [JsInt] ;; The block time expressed in UNIX epoch time
   time: [JsInt])) ;; Same as "blocktime"

(define-type ScriptPubKey
  (Record
   asm: [String] ;; the asm
   hex: [Bytes] ;; the hex
   reqSigs: [JsInt] ;; The required sigs
   type: [String] ;; The type, eg 'pubkeyhash'
   addresses: [(List String)])) ;; list of bitcoin addresses

(define-type BlockInfo0
  (Record
   hash: [Hash] ;; the block hash (same as provided)
   confirmations: [JsInt] ;; The number of confirmations, or -1 if the block is not on the main chain
   size: [JsInt] ;; The block size
   strippedsize: [JsInt] ;; The block size excluding witness data
   weight: [JsInt] ;; The block weight as defined in BIP 141
   height: [JsInt] ;; The block height or index
   version: [JsInt] ;; The block version
   versionHex: [Bytes] ;; The block version formatted in hexadecimal
   merkleroot: [Hash] ;; The merkle root
   time: [JsInt] ;; The block time expressed in UNIX epoch time
   mediantime: [JsInt] ;; The median block time expressed in UNIX epoch time
   nonce: [JsInt] ;; The nonce
   bits: [Bytes] ;; The bits
   difficulty: [Float] ;; The difficulty
   chainwork: [Bytes] ;; Expected number of hashes required to produce the chain up to this block (in hex) ;; TODO: have a type for hex-encoded bigints?
   nTx: [JsInt] ;; The number of transactions in the block
   previousblockhash: [(Maybe Hash)] ;; The hash of the previous block
   nextblockhash: [(Maybe Hash)])) ;; The hash of the next block

(define-type BlockInfo1
   (Record [BlockInfo0]
           tx: [(List Hash)])) ;; The transaction ids
(define-type BlockInfo2
   (Record [BlockInfo0]
           tx: [(List TxInfo)])) ;; The transactions

(def (getblock0 hash timeout: (timeout #f) log: (log btc-log))
  (<-json Hash (getblock hash 0 log: log timeout: timeout)))
(def (getblock1 hash timeout: (timeout #f) log: (log btc-log))
  (<-json BlockInfo1 (getblock hash 1 log: log timeout: timeout)))
(def (getblock2 hash timeout: (timeout #f) log: (log btc-log))
  (<-json BlockInfo2 (getblock hash 2 log: log timeout: timeout)))

(define-bitcoin-api chain getrawtransaction
  Json <- Bytes (Maybe Bool) (Maybe Hash))

(def (getrawtransaction/bytes txid (blockhash (void)) timeout: (timeout #f) log: (log btc-log))
  (<-json Bytes (getrawtransaction txid #f blockhash log: log timeout: timeout)))
(def (getrawtransaction/info txid (blockhash (void)) timeout: (timeout #f) log: (log btc-log))
  (<-json TxInfo (getrawtransaction txid #t blockhash log: log timeout: timeout)))


;;;; Util RPCs

;;; See https://developer.bitcoin.org/reference/rpc/getdescriptorinfo.html
;;; getdescriptorinfo "descriptor"
;;; bitcoin-cli getdescriptorinfo "wpkh([d34db33f/84h/0h/0h]0279be667ef9dcbbac55a06295Ce870b07029Bfcdb2dce28d959f2815b16f81798)"
(define-type DescriptorInfo
  (Record
   descriptor: [String] ;; The descriptor in canonical form, without private keys
   checksum: [String] ;; The checksum for the input descriptor
   isrange: [Bool] ;; Whether the descriptor is ranged
   issolvable: [Bool] ;; Whether the descriptor is solvable
   hasprivatekeys: [Bool])) ;; Whether the input descriptor contained at least one private key

;;; Analyses a descriptor.
(define-bitcoin-api keys getdescriptorinfo
  DescriptorInfo <- String)

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
(define-bitcoin-api keys deriveaddresses
  (List String) <- String Json)

(def (deriveaddress descriptor index timeout: (timeout #f) log: (log btc-log))
  (first-and-only (deriveaddresses descriptor index timeout: timeout log: log)))

(def (deriveaddressrange descriptor begin end timeout: (timeout #f) log: (log btc-log))
  (deriveaddresses descriptor [begin end] timeout: timeout log: log))

;;;; Wallet RPCs

;;; See https://developer.bitcoin.org/reference/rpc/signmessage.html
;;; signmessage "address" "message"
;;; Sign a message with the private key of an address.
;;; Requires wallet passphrase to be set with walletpassphrase call
;;; if wallet is encrypted.
(define-bitcoin-api keys signmessage
  String <- String String) ;; result in base64
