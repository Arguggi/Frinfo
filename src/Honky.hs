{-# LANGUAGE OverloadedStrings #-}

module Honky (main) where

import qualified Control.Concurrent as Conc
import           Control.Monad      (forever)
import           Control.Monad.Free
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Honky.Colors
import           Honky.Free
import           Honky.Scripts
import           Safe
import           System.IO
import qualified System.Process     as Process

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    -- remove newline
    uname <- (T.pack . initSafe) <$> Process.readProcess "uname" ["-r"] []
    printLoop' uname

printLoop' :: T.Text -> IO ()
printLoop' uname = forever $ do
    state <- getCpuStat
    Conc.threadDelay (secondsDelay 1)
    printDzen (freeStruc (State state) uname) >>= TIO.putStrLn

freeStruc :: State -> T.Text -> Free Dzen ()
freeStruc state uname = do
    icon headphoneColor "/home/arguggi/dotfiles/icons/xbm8x8/phones.xbm"
    script getSong
    separator
    icon ramColor "/home/arguggi/dotfiles/icons/stlarch/mem1.xbm"
    script getRam
    separator
    icon cpuColor "/home/arguggi/dotfiles/icons/stlarch/cpu1.xbm"
    scriptState getCpuAverage state
    separator
    icon unameColor "/home/arguggi/dotfiles/icons/xbm8x8/arch_10x10.xbm"
    static uname
    separator
    icon uptimeColor "/home/arguggi/dotfiles/icons/stlarch/logout1.xbm"
    script getUptime
    separator
    icon clockColor "/home/arguggi/dotfiles/icons/stlarch/clock1.xbm"
    script getTime

secondsDelay :: Int -> Int
secondsDelay x = x * 1000000
