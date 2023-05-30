(export #t)

(import
  :std/assert :std/sugar
  :std/srfi/13
  :clan/assert :clan/number
  ./json-rpc)

;;(define-type Keyspace String) ;; identifies the keyspace, e.g. one client company's CRM service.

;; BIP-32 path. (also glance at BIPs 43, 44, 49, 84 for conventions.)
;; https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
;; https://en.bitcoin.it/wiki/BIP_0032
;; m/seller-account'/change(0|1)/invoicee-index
;; for now we'll combine all sellers into a single account
;;(define-type BIP32-Path String) ;; type of BIP32 paths, e.g. "m/0'/0 xpub"
;;(def xpub/b32p "m/0'/0 xpub")

;; Descriptor for a key, to be fed into Bitcoind for processing, e.g. "wpkh([...]xpub/0/*)#chksum"
;;(define-type BitcoindKeyDescriptor String)

;; Keyspace to bitcoind key descriptor for the master public key for that keyspace
;; : (Table BitcoindKeyDescriptor <- Keyspace)
(def descriptor<-keyspace (hash))

;; Maximum index for normal BIP32 children
(def +max-bip32-index+ (1- (expt 2 31)))

;; Largest index published by a given keyspace
;; : (Table Integer <- Keyspace)
(def largest-published/table (hash))

;; customer ids aren't unique globally but are unique within a keyspace/xpub
;; when you make a qcust, ensure that there is an xpub for the keyspace
;; QCustomer
(defstruct qcust
  (keyspace ;; : Keyspace
   custid) ;; : Integer (?) ;; customer id within the keyspace
  transparent: #t)

(def (largest-published keyspace)
  (hash-ref largest-published/table keyspace -1))

;; DerivedAddress
(defstruct derived-address
  (index ;; : UInt32 ;; index
   address ;; : BitcoinAddress ;; actual address
   birthtime) ;; : (OrFalse UnixTimestamp) ;; creation
  transparent: #t)

;; : (Table BitcoinAddress Customer)
(def address<-customer/table (hash))

;;;; begin public

;; With address, print its expiry.
;; This is the time beyond which monitoring may stop and private key be deleted.

;; each qcust's der-addr
(def (address<-customer qcust)
  (hash-get address<-customer/table qcust))

;; : (OrFalse DerivedAddress) <- QCust UnixTimestamp
(def (try-add-customer qcust time)
  (unless (address<-customer qcust)
    (let* ((keyspace (qcust-keyspace qcust))
           (d (next-child keyspace)))
        (when (and d
                   (<= (1+ (largest-published keyspace))
                       (derived-address-index d)
                       +max-bip32-index+))
          (add-customer qcust d time)))))

;; TODO: in the UI, make sure always save how keys were generated, and report regular progress,
;; so that even if the rest of the data is lost, you can reconstitute the set of keys from the
;; master key and the counter.

;;;; end public

;;;; begin private

(def (next-child keyspace)
  (let loop ((i (1+ (largest-published keyspace)))
             (a #f))
    (if (or a (> i +max-bip32-index+))
      a
      (loop (1+ i) (derive-child keyspace i)))))

;; Derive a public child key from the keyspace's public master key as parent and an index
;; : DerivedAddress <- Keyspace UInt32
(def (derive-child keyspace i)
  ;; xpub's child at index i
  ;; see bitcoind's deriveaddresses RPC, where you pass in xpub.../<n>
  ;; https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md
  ;; "BIP32 derived keys and chains"
  (def address (fetch-address keyspace i))
  (and address (make-derived-address i address #f)))

(def (valid-address? address)
  ;;TODO "In case parse256(IL) ≥ n or Ki is the point at infinity, the resulting
  ;;      key is invalid, and one should proceed with the next value for i."
  ;;update: (a) apparently negligible probability,
  ;;        (b) bitcoin-cli deriveaddresses will produce error, from my reading.
  #t)

;;;;
;"When importing a serialized extended public key, implementations must verify whether the X coordinate in the public key data corresponds to a point on the curve. If not, the extended public key is invalid."
;
;Is this the same as rejecting indices that produce invalid children?
;Section: "Public parent key -> public child key"
;If so, then that's OK (assuming you'll check that during generation).
;If not, then you shouldn't publish address on invoice before importing it successfully into bitcoind (i.e. checking import produces a successful response or whatever).
;;;;

;; Signals an error if bitcoinc fails unexpectedly
;; : (OrFalse BitcoinAddress) <- Keyspace UInt32
(def (fetch-address keyspace i)
  (def a (derive-address (hash-get descriptor<-keyspace keyspace) i))
  (and (valid-address? a) a))

(def (derive-address descriptor i)
  ;;TODO this call must be to a bitcoind deployment trustworthy enough
  ;;     for address derivation, not to some public block notifier.
  (assert! (string-prefix? "wpkh" descriptor))
  (deriveaddress descriptor i))

; state changes are isolated here for prevayler
(def (add-customer qcust derived-address time)
  (let ((keyspace (qcust-keyspace qcust)))
    (hash-put! largest-published/table keyspace
               (max (largest-published keyspace)
                    (derived-address-index derived-address)))
    (set! (derived-address-birthtime derived-address) time)
    (hash-put! address<-customer/table qcust derived-address)))

;;;; end private
