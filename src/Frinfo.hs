{-# LANGUAGE OverloadedStrings #-}

module Frinfo
        ( main
        , freeStruc
        ) where

import qualified Control.Concurrent         as Conc
import           Control.Monad.Free
import qualified Control.Monad.State.Strict as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Frinfo.Colors
import           Frinfo.DBus
import           Frinfo.Free
import           Frinfo.Scripts
import           Safe
import           System.IO
import qualified System.Process             as Process

-- |The main function must build a new 'StaticState' that will remain unchanged
-- and will be used for the duretion of the program
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- remove newline
    unameIO <- (T.pack . initSafe) <$> Process.readProcess "uname" ["-r"] []
    mvar <- connectToDbus
    let startingState =  MyState (SystemState [defaultCpuStat] [defaultNetStat] mvar) (StaticState unameIO)
    printLoop startingState

-- |The loops that keeps printing the system info
printLoop :: MyState -> IO ()
printLoop state = do
    (output, newState) <- S.runStateT (printDzen freeStruc) state
    TIO.putStrLn output
    Conc.threadDelay (secondsDelay 1)
    printLoop newState

-- |Build the data structure that will then be 'interpreted'
-- See 'Info' for the available constructors
freeStruc :: Free Info ()
freeStruc = do
    icon headphoneColor "/home/arguggi/dotfiles/icons/xbm8x8/phones.xbm"
    scriptState getSong
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

-- | Convert number to seconds
secondsDelay :: Int -> Int
secondsDelay x = x * 1000000
