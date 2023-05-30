#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first running ./build.ss !

(import :clan/testing :clan/path-config :clan/path)

;; Initialize the test environment, and enable the associated command-line subcommands.
;; This also enables the loading of modules below even when not compiled.
(init-test-environment!)

;; Accept the command-line subcommands defined in the files below.
(import :mukn/bitcoin/version :mukn/bitcoin/cli)

;; Define more commands
(import :std/misc/process
        :clan/multicall)

(define-entry-point (build-and-test)
  (help: "Run all build and test commands" getopt: [])
  (run-process/batch ["./build.ss"])
  (run-process/batch ["./unit-tests.ss"])
  #;(run-process/batch ["./scripts/run-bitcoin-test-net.ss"])
  (run-process/batch ["./unit-tests.ss" "integration"])
  #;(run-process/batch ["./scripts/run-bitcoin-test-net.ss" "stop"]))
