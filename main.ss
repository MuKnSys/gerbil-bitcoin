(import
  ./json-rpc ./cmd ./cli)

#| how to run
; set up tunnels for slack http proxying
; run bitcoind
; tunnel to bitcoind
(pushnew :drakma-no-ssl *features*) ;drakma's util will then need recompiling
(ql:quickload "btcpay")
(in-package btcpay)
(load "conf") ; before initialize replays changelog
(initialize)
(unless *lastblkhash* (cmd-poll-chain))
(assert *lastblkhash*)
(main-loop) ; you can bounce in and out of this
; if you reload the program, then also reload conf
|#

;;;; exports: main-loop
;;;; expects: journal's initialize to have been called

(def *chain-poll-sec* 10)
(def *slack-process-sec* 3)
(def *slack-outgoing-sec* 3)
(def *changelog-lax-sec* (* 60 5))
(def *announcements-sec* 3)

(def (flush-line (s #f)) (newline) (when s (display s)) (force-output))
(def (prompt) (flush-line "> "))

(def (main-loop)
  (assert *lastblkhash*)
  (assert-changes-file-open)
  (srv-open *slack-srv*)
  (unwind-protect
    (main-loop1)
    (initiate-flush-changes)
    (srv-close *slack-srv*)))

(def (main-loop1)
  (write-line "Started Main Loop. Input :terminate to terminate it.")
  (prompt)
  (let ((start (get-universal-time)))
    (let ((next-chain start)
          (next-ann start)
          (next-slack-process start)
          (next-slack-outgoing start)
          (next-flush (+ start *changelog-lax-sec*)))
      (do ((now start (get-universal-time))) (nil)
        (when (>= now next-chain)
          (cmd-poll-chain)
          (setf next-chain (+ now *chain-poll-sec*)))
        (when (>= now next-ann)
          (cmd-part-announcements)
          (cmd-cli-process-announcements)
          (cmd-sl-process-announcements)
          (setf next-ann (+ now *announcements-sec*)))
        (when (>= now next-flush)
          (initiate-flush-changes)
          (setf next-flush (+ now *changelog-lax-sec*)))
        (when (>= now next-slack-process)
          (cmd-process-sl-in)
          (setf next-slack-process (+ now *slack-process-sec*)))
        (when (>= now next-slack-outgoing)
          (cmd-process-sl-mouts)
          (setf next-slack-outgoing (+ now *slack-outgoing-sec*)))
        (when (listen)
          (restart-case
              (let ((c (read-char-no-hang)))
                (when (graphic-char-p c)
                  (unread-char c)
                  ; main loop is paused during multi-line read or debugger entry
                  (let ((expr (read)))
                    (case expr
                      ((:terminate) (return))
                      (otherwise (print (eval expr)) (prompt))))))
            (application-loop ())))
        (srv-poll *slack-srv* 1)
        ;(sleep 2)
        (void)))))
