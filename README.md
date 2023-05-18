# Gerbil-Bitcoin

Library to support Bitcoin blockchain operations from Gerbil Scheme

License: Apache License 2.0

## Testing the code

### Running bitcoind in test mode

Start a local `bitcoind` in regression test mode with:

```
bitcoind -regtest -daemon -fallbackfee=1.0 -maxtxfee=1.1
```

It will listen on ports 18443, 18444, 18445, and keep information under `~/.bitcoin/regtest/`.

TODO: Learn from https://github.com/BlockchainCommons/Learning-Bitcoin-from-the-Command-Line/blob/master/A3_0_Using_Bitcoin_Regtest.md and import any useful information.

### Querying bitcoind

You can then query the regression test bitcoin network using `bitcoin-cli -regtest`.

```
bh="$(bitcoin-cli -regtest getbestblockhash)"
echo $bh
bitcoin-cli -regtest getblock $bh
```

To stop bitcoind running, execute:
```
bitcoin-cli -regtest stop
```

## Shared test environment

We ought to setup something on our development/test server mallory.
