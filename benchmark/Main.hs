{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Concurrent         as Conc
import Criterion (bench, bgroup)
import Criterion.Main (defaultMain, nfIO)
import qualified Frinfo                     as F
import qualified Frinfo.Free                as FF
import qualified Frinfo.INotify             as IN
import qualified Control.Monad.State.Strict as S

main :: IO ()
main = do
    songMVar <- Conc.newMVar "Test Song"
    emailMVar <- Conc.newMVar 0
    let sState = FF.systemState FF.defaultMyState
        fresStrucMvar = FF.defaultMyState { FF.systemState = sState { FF.dbusState = songMVar, FF.emailState = emailMVar }}
    defaultMain
        [ bgroup "frinfo"
            [ bench "print Dzen" $ nfIO (S.evalStateT (FF.printDzen F.freeStruc) fresStrucMvar)]
        , bgroup "INotify"
            [ bench "new Folders" $ nfIO IN.allNewFolders
            , bench "total Files" $ nfIO $ IN.getTotalFiles "/home/arguggi/Mail/arguggi@arguggi.co.uk/INBOX/new"]
        ]
