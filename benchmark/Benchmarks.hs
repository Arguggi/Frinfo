{-# LANGUAGE OverloadedStrings #-}

module Benchmarks (main) where

import Criterion (bench, bgroup)
import Criterion.Main (defaultMain, nfIO)
import qualified Honky as H
import qualified Control.Monad.State.Strict as S

main :: IO ()
main = defaultMain [
    bgroup "honky" [bench "print Dzen" $ nfIO (S.evalStateT (H.exportPrintDzen H.freeStruc) H.defaultMyState)]
    ]
