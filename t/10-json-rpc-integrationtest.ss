(export #t)

(import :std/test :clan/debug
        ../json-rpc)

;; Use our Private Ethereum Testnet
(ensure-test-bitcoin-connection)

(def 10-json-rpc-integrationtest
  (test-suite "integration test for bitcoin/json-rpc"
    (test-case "get the current latest block"
      ;; Dump information about the Ethereum JSON RPC configuration
      (DBG json-rpc-integrationtest:
           bitcoind-chain bitcoind-keys
           (bc-bestblockhash))
      ;; Just checks that the block number is non-negative
      (check (bc-bestblockhash) ? identity))))
