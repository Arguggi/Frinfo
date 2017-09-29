{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frinfo.Free
    ( CpuStat(..)
    , NetStat(..)
    , interface
    , downTotal
    , upTotal
    , Info(..)
    , separator
    , icon
    , static
    , staticIO
    , script
    , scriptState
    , StaticState(..)
    , uname
    , dbusState
    , emailState
    , SystemState(..)
    , cpuState
    , netState
    , wrapIcon
    , wrapColor
    , runFree
    , printDzen
    ) where

import Control.Lens
import Control.Monad.Free (Free(..), liftF, MonadFree)
import Control.Monad.Free.Church (F)
import qualified Control.Monad.Free.Church as Church
import Control.Monad.Free.TH (makeFree)
import Control.Monad.Reader (ReaderT(..), MonadReader, ask)
import Control.Monad.State (StateT(..), MonadState, MonadIO, get, liftIO, put)
import Data.Default (Default(), def)
import Data.Monoid ((<>))
import qualified Control.Concurrent as Conc
import qualified Control.Monad.State.Strict as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Frinfo.Config as Config

type Path = T.Text

-- | Cpu data taken from @\/proc\/stat@
data CpuStat = CpuStat
    { _user :: Integer
    -- ^User stats
    , _system :: Integer
    -- ^System stats
    , _idle :: Integer
    -- ^Idle stats
    } deriving (Show)

-- | Interface data taken from @\/proc\/net\/dev@
data NetStat = NetStat
    { _interface :: T.Text
    -- ^ Inteface name (e.g. enp5s0)
    , _downTotal :: Integer
    -- ^ Total bits downloaded
    , _upTotal :: Integer
    -- ^ Total bits uploaded
    } deriving (Show)

-- | Dynamic state that will be used with the 'ScriptState' constructor
data SystemState = SystemState
    { _cpuState :: [CpuStat]
    -- ^ List of all 'CpuStat', one for each core and one for the total average
    , _netState :: [NetStat]
    -- ^ List of all 'NetStat', one for each interface
    }

-- | Static state that must be set at startup in 'main'
data StaticState = StaticState
    { _uname :: T.Text
    -- ^ Output of @uname -r@
    , _dbusState :: Conc.MVar T.Text
    -- ^ Currently playing song
    , _emailState :: Conc.MVar Int
    -- ^ Number of unread emails
    }

data Info next
    = Separator next
      -- ^ Add a simple separator, defined as 'sep'
    | Icon Config.Color
           Path
           next
      -- ^ Add an icon, with a 'Config.Color' and 'Path'
    | Static (StaticState -> T.Text) next
      -- ^ Add some fixed text. You have to specify the function
      -- used to extract the 'T.Text' from the 'StaticState' data
    | StaticIO (StaticState -> IO T.Text) next
      -- ^ Add some 'IO' 'T.Text' that needs 'StaticState'.
    | Script (IO T.Text) next
      -- ^ Add some 'IO' 'T.Text'.
    | ScriptState (StateT SystemState IO T.Text) next
      -- ^ Add some 'IO' 'T.Text' that also needs access to the previous 'SystemState'
    deriving (Functor)

makeFree ''Info

makeLenses ''StaticState

makeLenses ''SystemState

makeLenses ''NetStat

newtype FrinfoM a = Frinfo { runFrinfo :: ReaderT StaticState (StateT SystemState IO) a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader StaticState
        , MonadState SystemState
        )

type Frinfo = FrinfoM TLB.Builder


instance Ord NetStat where
    compare (NetStat _ d1 _) (NetStat _ d2 _) = compare d2 d1

instance Eq NetStat where
    (NetStat _ d1 _) == (NetStat _ d2 _) = d1 == d2

instance Default CpuStat where
    def = CpuStat 0 0 0

instance Default NetStat where
    def = NetStat "Empty" 0 0

instance Default StaticState where
    def = StaticState "uname" undefined undefined

instance Default SystemState where
    def = SystemState [def] [def]

-- | Separator that is used when the 'Separator' constructor is used
sep :: T.Text
sep = " | "

printDzen :: (MonadIO m, MonadReader StaticState m, MonadState SystemState m) => F Info TLB.Builder -> m TLB.Builder
printDzen = Church.iterM buildDzen

-- | Interpret the Info + Free Monad.
-- This outputs 'IO' ('T.Text', 'MyState').
-- The 'T.Text' should be fed to dzen2.
-- The 'MyState' should be used in the next print statement
buildDzen :: (MonadIO m, MonadReader StaticState m, MonadState SystemState m) => Info (m TLB.Builder) -> m TLB.Builder
buildDzen (Separator next) = do
    rest <- next
    return $ TLB.fromText sep <> rest
buildDzen (Icon color path next) = do
    let iconText = wrapIcon path
        wrappedIcon = wrapColor color iconText
    rest <- next
    return $ TLB.fromText wrappedIcon <> rest
buildDzen (Static getStatic next) =
    buildDzen (StaticIO (return . getStatic) next)
buildDzen (StaticIO ioScript next) = do
    rest <- next
    sstate <- ask
    staticTextIO <- liftIO $ ioScript sstate
    return $ TLB.fromText staticTextIO <> rest
buildDzen (Script ioScript next) = do
    output <- S.liftIO ioScript
    rest <- next
    return $ TLB.fromText output <> rest
buildDzen (ScriptState ioScript next) = do
    sState <- get
    (output, nState) <- liftIO $ runStateT ioScript sState
    put nState
    rest <- next
    return $ TLB.fromText output <> rest

-- Utility wrappers
-- | Wrap some text in a color
wrapColor :: Config.Color -> T.Text -> T.Text
wrapColor color text = "^fg(" <> color <> ")" <> text <> "^fg()"

-- | Wrap some path so that it will interpreted like an image by dzen
wrapIcon :: T.Text -> T.Text
wrapIcon path = "^i(" <> path <> ") "

runFree :: StaticState -> SystemState -> Frinfo -> IO (TLB.Builder, SystemState)
runFree rState sState = flip runStateT sState . flip runReaderT rState . runFrinfo
