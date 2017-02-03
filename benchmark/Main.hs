{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Concurrent         as Conc
import Criterion (bench, bgroup)
import Criterion.Main (defaultMain, nfIO)
import Data.Default (def)
import qualified Frinfo.Structure           as F
import qualified Frinfo.Free                as FF
import qualified Frinfo.INotify             as IN
import qualified Control.Monad.State.Strict as S

main :: IO ()
main = do
    songMVar <- Conc.newMVar "Test Song"
    emailMVar <- Conc.newMVar 0
    let sState = FF._systemState (def :: FF.MyState)
        fresStrucMvar = (def :: FF.MyState) { FF._systemState = sState { FF._dbusState = songMVar, FF._emailState = emailMVar }}
    defaultMain
        [ bgroup "frinfo"
            [ bench "Print Dzen" $ nfIO (S.evalStateT (FF.printDzen F.freeStruc) fresStrucMvar)]
        , bgroup "INotify"
            [ bench "New Folders" $ nfIO IN.allNewFolders
            , bench "Total Files" $ nfIO $ IN.getTotalFiles "/home/arguggi/Downloads"
            ]
        ]
