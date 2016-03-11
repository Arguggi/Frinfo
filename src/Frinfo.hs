{-# LANGUAGE OverloadedStrings #-}

module Frinfo
        ( main
        , freeStruc
        ) where

import qualified Control.Concurrent         as Conc
import           Control.Monad
import           Control.Monad.Free
import qualified Control.Monad.State.Strict as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Frinfo.Config              as Config
import           Frinfo.DBus
import           Frinfo.Free
import           Frinfo.MPD
import           Frinfo.INotify
import           Frinfo.Scripts
import           Options.Applicative
import           Safe
import           System.IO
import qualified System.Process             as Process

data Flags = Flags
    { mpd :: Bool
    , spotify :: Bool
    , inotify :: Bool
    }

options :: Parser Flags
options = Flags
    <$> switch
        ( long "mpd"
        <> help "Keep getting song info from MPD")
    <*> switch
        ( long "spotify"
        <> help "Keep getting song info from Spotify")
    <*> switch
        ( long "inotify"
        <> help "Watch for new files in the email folder. (Default ~/Mail/)")

helpOpts :: ParserInfo Flags
helpOpts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Print system information to stdout"
    <> header "Frinfo")

-- | The main function must build a new 'StaticState' that will remain unchanged
-- and will be used for the duration of the program
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    flags <- execParser helpOpts
    -- remove newline
    unameIO <- (T.pack . initSafe) <$> Process.readProcess "uname" ["-r"] []
    songMVar <- Conc.newMVar Config.noSongPlaying
    emailMVar <- Conc.newMVar 0
    when (mpd flags)     (void . Conc.forkIO $ connectToMPD songMVar)
    when (spotify flags) (void . Conc.forkIO $ connectToDbus songMVar)
    when (inotify flags) (void . Conc.forkIO $ watchEmailFolder emailMVar)
    let startingState = MyState dynamicState staticState'
        dynamicState = SystemState { cpuState = [defaultCpuStat]
                                   , netState = [defaultNetStat]
                                   , dbusState = songMVar
                                   , emailState = emailMVar
                                   }
        staticState' = StaticState { uname = unameIO }
    printLoop startingState

-- | The loop that keeps printing the system info
printLoop :: MyState -> IO ()
printLoop state = do
    (output, newState) <- S.runStateT (printDzen freeStruc) state
    TIO.putStrLn output
    Conc.threadDelay (secondsDelay 1)
    printLoop newState

-- | Build the data structure that will then be 'interpreted'
-- See 'Info' for the available constructors
freeStruc :: Free Info ()
freeStruc = do
    icon Config.headphoneColor Config.songIcon
    scriptState getSong
    separator
    icon Config.emailColor Config.emailIcon
    scriptState getUnreadEmails
    separator
    script getRam
    separator
    scriptState getNetAverage
    icon Config.upColor Config.upSpeedIcon
    separator
    scriptState getCpuAverage
    script getCpuRpm
    separator
    icon Config.unameColor Config.unameIcon
    static uname
    separator
    script getUptime
    separator
    script getTime

-- | Convert number to seconds
secondsDelay :: Int -> Int
secondsDelay x = x * 1000000
