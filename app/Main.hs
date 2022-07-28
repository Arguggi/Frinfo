{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Lens ((^.))
import Control.Monad (when, void)
import Data.Default (def)
import Data.Time (getCurrentTime)
import Frinfo.DBus
import Frinfo.Free
import Frinfo.INotify
import Frinfo.MPD
import Frinfo.Structure
import Options.Applicative (Parser, ParserInfo, switch, long, help, header, info, helper, fullDesc, progDesc, execParser)
import Safe (initSafe)
import SlaveThread (fork)
import System.Directory (createDirectoryIfMissing, getXdgDirectory, withCurrentDirectory, XdgDirectory(XdgData))
import System.IO (withFile, IOMode(..), hPutStrLn, hSetBuffering, stdout, BufferMode(..))

import qualified Control.Concurrent as Conc
import qualified Control.Exception as Ex
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO
import qualified Frinfo.Config as Config
import qualified System.Process as Process

data Flags = Flags
    { mpd :: Bool
    , spotify :: Bool
    , inotify :: Bool
    }

options :: Parser Flags
options =
    Flags <$> switch (long "mpd" <> help "Keep getting song info from MPD") <*>
    switch (long "spotify" <> help "Keep getting song info from Spotify") <*>
    switch
        (long "inotify" <>
         help "Watch for new files in the email folder. (Default ~/Mail/)")

helpOpts :: ParserInfo Flags
helpOpts =
    info
        (helper <*> options)
        (fullDesc <> progDesc "Print system information to stdout" <>
         header "Frinfo")

-- | Log any exceptions to 'Config.crashFile'
logException :: Ex.SomeException -> IO ()
logException e = do
    time <- getCurrentTime
    let errorLine = show time <> " - " <> show e
    print e
    xdgData <- getXdgDirectory XdgData "frinfo"
    createDirectoryIfMissing True xdgData
    withCurrentDirectory xdgData $
        withFile Config.crashFileName AppendMode $
            flip hPutStrLn errorLine

-- | The loop that keeps printing the system info
printLoop :: StaticState -> SystemState -> IO ()
printLoop staticS systemS = do
    (output, newSystemS) <- runFree staticS systemS (printDzen freeStruc)
    TLIO.putStrLn . TLB.toLazyText $ output
    Conc.threadDelay 500000
    printLoop staticS newSystemS

-- | 'Ex.catch'-es all exceptions with 'Frinfo.logException'
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    flags <- execParser helpOpts
    (staticS, dynamicS) <- initialState
    main' flags staticS dynamicS `Ex.catch` logException

-- | The main' function must build a new 'StaticState' that will remain unchanged
-- and will be used for the duration of the program
main' :: Flags -> StaticState -> SystemState -> IO ()
main' flags staticS systemS = do
    let songMVar = staticS ^. dbusState
        emailMVar = staticS ^. emailState
    when (mpd flags) $ void (fork (connectToMPD songMVar))
    when (spotify flags) $ void (fork (connectToDbus songMVar))
    when (inotify flags) $ void (fork (watchEmailFolder emailMVar))
    printLoop staticS systemS

initialState :: IO (StaticState, SystemState)
initialState = do
    unameIO <- T.pack . initSafe <$> Process.readProcess "uname" ["-r"] []
    songMVar <- Conc.newMVar Config.noSongPlaying
    emailMVar <- Conc.newMVar 0
    -- remove newline
    let dynamicState =
            SystemState
            { _cpuState = [def]
            , _netState = [def]
            }
        staticState' =
            StaticState
            { _uname = unameIO
            , _dbusState = songMVar
            , _emailState = emailMVar
            }
    return (staticState', dynamicState)
