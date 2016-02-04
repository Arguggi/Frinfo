{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion (bench, nf, bgroup, Benchmark)
import Criterion.Main (defaultMain, nfIO)
import qualified Data.Text          as T
import qualified Honky as H

main :: IO ()
main = defaultMain [
    bgroup "honky" [bench "test" $ nfIO (H.exportPrintDzen (H.freeStruc H.exportState ("uname" :: T.Text)))]
    ]
