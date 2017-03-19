{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Control.Concurrent as Conc
import qualified Control.Exception as Ex
import Control.Lens ((^.))
import Control.Monad
import qualified Control.Monad.State.Strict as S
import Data.Default (def)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time
import qualified Frinfo.Config as Config
import Frinfo.DBus
import Frinfo.Free
import Frinfo.INotify
import Frinfo.MPD
import Frinfo.Structure
import Options.Applicative
import Safe
import System.IO
import qualified System.Process as Process
import SlaveThread (fork)

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
    putStrLn "Terminating"
    time <- getCurrentTime
    let errorLine = show time <> " - " <> show e
    withFile Config.crashFile AppendMode $ flip hPutStrLn errorLine

-- | The loop that keeps printing the system info
printLoop :: MyState -> IO ()
printLoop state = do
    (output, newState) <- S.runStateT (printDzen freeStruc) state
    TLIO.putStrLn . TLB.toLazyText $ output
    Conc.threadDelay (secondsDelay 1)
    printLoop newState

-- | 'Ex.catch'-es all exceptions with 'Frinfo.logException'
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    flags <- execParser helpOpts
    initState <- initialStaticState
    main' flags initState `Ex.catch` logException

-- | The main' function must build a new 'StaticState' that will remain unchanged
-- and will be used for the duration of the program
main' :: Flags -> MyState -> IO ()
main' flags initState = do
    let songMVar = initState ^. systemState . dbusState
        emailMVar = initState ^. systemState . emailState
    when (mpd flags) $ void (fork (connectToMPD songMVar))
    when (spotify flags) $ void (fork (connectToDbus songMVar))
    when (inotify flags) $ void (fork (watchEmailFolder emailMVar))
    printLoop initState

initialStaticState :: IO MyState
initialStaticState = do
    unameIO <- (T.pack . initSafe) <$> Process.readProcess "uname" ["-r"] []
    songMVar <- Conc.newMVar Config.noSongPlaying
    emailMVar <- Conc.newMVar 0
    -- remove newline
    let startingState = MyState dynamicState staticState'
        dynamicState =
            SystemState
            { _cpuState = [def]
            , _netState = [def]
            , _dbusState = songMVar
            , _emailState = emailMVar
            }
        staticState' =
            StaticState
            { _uname = unameIO
            }
    return startingState

-- | Convert number to seconds
secondsDelay :: Int -> Int
secondsDelay x = x * 1000000
