{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Concurrent         as Conc
import Criterion (bench, bgroup)
import Criterion.Main (defaultMain, nfIO)
import qualified Frinfo                     as F
import qualified Frinfo.Free                as FF
import qualified Control.Monad.State.Strict as S

main :: IO ()
main = do
    mvar <- Conc.newMVar "Test Song"
    let sState = FF.systemState FF.defaultMyState
        fresStrucMvar = FF.defaultMyState { FF.systemState = sState { FF.dbusState = mvar }}
    defaultMain [
        bgroup "frinfo" [bench "print Dzen" $ nfIO (S.evalStateT (FF.printDzen F.freeStruc) fresStrucMvar)]
        ]
