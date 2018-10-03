# nano-cryptr

[![Hackage](https://img.shields.io/hackage/v/nano-cryptr.svg)](https://hackage.haskell.org/package/nano-cryptr) [![Build Status](https://travis-ci.com/janrain/nano-cryptr.svg?branch=master)](https://travis-ci.com/janrain/nano-cryptr)

This is a simple, thread-safe Haskell binding to glibc's crypt_r function.

```
> import System.Gnu.CryptR
> cryptR "REDACTED" "$6$4VMgp/9O$"
Just "$6$4VMgp/9O$O3uYU1gtPUrJkTIDwWtylzztmaHiwUO/KsK9d6QpAvMUOVbSeYy5DY4lxO6YZJoakJhwAgB2H406paso6KPpR/"
```

Refer to the [package documentation](https://hackage.haskell.org/package/nano-cryptr)
for information on how to use the provided bindings, and check the test
directory for examples.

For information on glibc's crypt_r format, please [read the manual](http://man7.org/linux/man-pages/man3/crypt.3.html).
