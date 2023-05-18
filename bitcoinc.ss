(export #t)

;; See this reference documentation (that explicitly disclaims being a specification):
;; https://developer.bitcoin.org/reference/rpc/

(import
  :clan/net/json-rpc
  ./http-ka)

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
  (url ;; URL ;;
   auth) ;; : Auth ;; auth argument for std/net/request, e.g. #f or [basic: "myusername" "mypassword"]
  transparent: #t)

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
(def (bitcoind-rpc connection method . params)
  (with ((BitcoindConnection url auth) connection)
    (json-rpc url method params auth: auth)))

(def (bitcoind-chain-rpc method . params)
  (apply bitcoind-rpc bitcoind-chain method params))

(def (bitcoind-keys-rpc method . params)
  (apply bitcoind-rpc bitcoind-keys method params))


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
  (bitcoind-chain-rpc "getblock" hash 2))

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
  (bitcoind-keys-rpc "getdescriptorinfo" prevalidated-desc))

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
  (bitcoind-keys-rpc "deriveaddresses" descriptor (list index index)))

;;;; Wallet RPCs

;;; See https://developer.bitcoin.org/reference/rpc/signmessage.html
;;; signmessage "address" "message"
;;; Sign a message with the private key of an address.
;;; Requires wallet passphrase to be set with walletpassphrase call
;;; if wallet is encrypted.
(def (bc-sign address message)
  (bitcoind-keys-rpc "signmessage" address message))
