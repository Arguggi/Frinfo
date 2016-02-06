{-# LANGUAGE OverloadedStrings #-}

module Honky (main, freeStruc, defaultMyState, exportPrintDzen) where

import qualified Control.Concurrent         as Conc
import           Control.Monad.Free
import qualified Control.Monad.State.Strict as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Honky.Colors
import           Honky.Free
import           Honky.Scripts
import           Safe
import           System.IO
import qualified System.Process             as Process

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    -- remove newline
    unameIO <- (T.pack . initSafe) <$> Process.readProcess "uname" ["-r"] []
    let startingState =  MyState (SystemState [defaultCpuStat] [defaultNetStat]) (StaticState unameIO)
    printLoop startingState

printLoop :: MyState -> IO ()
printLoop state = do
    (output, newState) <- S.runStateT (printDzen freeStruc) state
    TIO.putStrLn output
    Conc.threadDelay (secondsDelay 1)
    printLoop newState

freeStruc :: Free Dzen ()
freeStruc = do
    icon headphoneColor "/home/arguggi/dotfiles/icons/xbm8x8/phones.xbm"
    script getSong
    separator
    icon ramColor "/home/arguggi/dotfiles/icons/stlarch/mem1.xbm"
    script getRam
    separator
    scriptState getNetAverage
    icon upColor "/home/arguggi/dotfiles/icons/xbm8x8/net_up_03.xbm"
    separator
    icon cpuColor "/home/arguggi/dotfiles/icons/stlarch/cpu1.xbm"
    scriptState getCpuAverage
    script getCpuRpm
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

exportPrintDzen :: Free Dzen () -> StateM
exportPrintDzen = printDzen
