{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Concurrent as Conc
import Criterion (bench, bgroup)
import Criterion.Main (defaultMain, nfIO)
import Data.Default (def)
import qualified Frinfo.Structure as F
import qualified Frinfo.Free as FF
import qualified Frinfo.INotify as IN
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

main :: IO ()
main = do
    songMVar <- Conc.newMVar "Test Song"
    emailMVar <- Conc.newMVar 0
    let staticS = def :: FF.StaticState
        dynamicS = def :: FF.SystemState
        stat = staticS { FF._dbusState = songMVar, FF._emailState = emailMVar}
    defaultMain
        [ bgroup "frinfo"
            [ bench "PrintDzen" $ nfIO $ do
                (t, _) <- FF.runFree stat dynamicS (FF.printDzen F.freeStruc)
                return . TL.toStrict . TLB.toLazyText $ t
            ]
        , bgroup "INotify"
            [ bench "NewFolders" $ nfIO IN.allNewFolders
            , bench "TotalFiles" $ nfIO $ IN.getTotalFiles "/home/arguggi/Downloads"
            ]
        ]
