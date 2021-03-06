# Frinfo - Print system info to stdout

[![Build Status](https://travis-ci.org/Arguggi/Frinfo.svg?branch=master)](https://travis-ci.org/Arguggi/Frinfo)
[![licence](https://img.shields.io/badge/license-MIT-green.svg?dummy)](https://opensource.org/licenses/MIT)

## Build

Builing with stack is recommended:
```
$ git clone https://github.com/Arguggi/Frinfo.git
$ cd Frinfo
$ stack build
```
## How-to

The output of `frinfo` is meant to be piped to dzen:
```
$ frinfo | dzen2
```

To see which features are available:
```
$ frinfo --help
```

## Target OS

`Frinfo` only works on `linux` since it reads system info from `/proc/` files.

## Haskell

`Frinfo` is mostly a learning excercise. It builds up a data structure using the `Free`
monad which is then interpreted and printed.

Since some statistics need to average the current state with a previous state the
`StateT` monad transformer is used to eliminate some boilerplate.

Don't expect particularly idiomatic Haskell code, this is also still a WIP.
