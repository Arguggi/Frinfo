{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion (bench, bgroup)
import Criterion.Main (defaultMain, nfIO)
import qualified Frinfo as F
import qualified Control.Monad.State.Strict as S

main :: IO ()
main = defaultMain [
    bgroup "frinfo" [bench "print Dzen" $ nfIO (S.evalStateT (F.exportPrintDzen F.freeStruc) F.defaultMyState)]
    ]
