# Gerbil-Bitcoin

Library to support Bitcoin blockchain operations from Gerbil Scheme

License: Apache License 2.0

## WIP

Beware, this is an early work in progress,
that hasn't been used or tested much yet.
It is based on a prototype written by Vibhu Mohindra
in Common Lisp for Mutual Knowledge Systems,
and translated to Scheme by François-René Rideau.
Very little is here, and already large parts of what is here
needs to be rewritten.

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

On a machine where you only or mostly use the regtest server,
you can add a `regtest=1` line in your `~/.bitcoin/bitcoin.conf` file
and not have to specify `-regtest` at the command-line anymore
(instead having to use `-chain=main` when you want to talk to a "real" bitcoin).
