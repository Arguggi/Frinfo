#!/usr/bin/env bash
find ./src/ -iname "*.hs" -type f -exec stylish-haskell -i {} \; -exec hindent --indent-size 4 --no-force-newline {} \;
