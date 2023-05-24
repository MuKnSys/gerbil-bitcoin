
#|
(def (bitcoin-armored plaintext address signature)
  (with-output-to-string (out)
    (bitcoin-armor plaintext address signature out)))

; out-stream is assumed buffered
(def (bitcoin-armor plaintext address signature out-stream)
  "Ref: https://en.bitcoin.it/wiki/Message_signing
        https://bitcointalk.org/index.php?topic=241802.0
        which describe a PGP armour style for bitcoin signed messages."
  (labels ((lf () (write-char #\Linefeed out-stream)))
    (write-sequence "-----BEGIN BITCOIN SIGNED MESSAGE-----" out-stream) (lf)
    (write-dash-escaped plaintext out-stream) (lf)
    (write-sequence "-----BEGIN BITCOIN SIGNATURE-----" out-stream) (lf)
    (write-sequence address out-stream) (lf)
    (write-sequence signature out-stream) (lf)
    (write-sequence "-----END BITCOIN SIGNATURE-----" out-stream))
  (values))

(def (write-dash-escaped text out-stream)
  "Lines that start with a dash are escaped by prefixing a dash and space.
   Ref: RFC 2440 section 7.1"
  (do ((len (length text))
       (newline t)
       (i 0 (1+ i)))
      ((>= i len))
    (let ((c (char text i)))
      (when (and newline (eql c #\-))
        (write-sequence "- " out-stream))
      (write-char c out-stream)
      (setf newline (eql c #\Linefeed)))))

;;;;

(def (bitcoin-asign address plaintext out-stream)
  (bitcoin-armor plaintext
                 address
                 (bc-sign address plaintext)
                 out-stream))

; bitcoin-cli's getnewaddress can be asked for a legacy (i.e. P2PKH) address
; that can be used for signing.
(def (bitcoin-asigned address plaintext)
  (with-output-to-string (o)
    (bitcoin-asign address plaintext o)))

;;;;

; bitcoin-cli getnewaddress "whateverlabel" legacy
; will create one for you
(defvar *signing-address* nil)

(def (sign plaintext)
  (bitcoin-asigned *signing-address* plaintext))
|#
