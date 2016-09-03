{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Frinfo.Free where

import qualified Control.Concurrent         as Conc
import           Control.Lens
import           Control.Monad.Free
import qualified Control.Monad.State.Strict as S
import           Data.Default
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Frinfo.Config              as Config

type Path = T.Text

data Info next =
      Separator next
      -- ^ Add a simple separator, defined as 'sep'
    | Icon Config.Color Path next
      -- ^ Add an icon, with a 'Color' and 'Path'
    | Static (StaticState -> T.Text) next
      -- ^ Add some fixed text. You have to specify the function
      -- used to extract the 'T.Text' from the 'StaticState' data
    | Script (IO T.Text) next
      -- ^ Add some 'IO T.text'.
    | ScriptState (SystemState -> IO (T.Text, SystemState)) next
      -- ^ Add some 'IO Text' that also needs access to the previous 'SystemState'
    deriving (Functor)

-- | Cpu data taken from @\/proc\/stat@
data CpuStat = CpuStat
    { _user   :: Integer
    -- ^User stats
    , _system :: Integer
    -- ^System stats
    , _idle   :: Integer
    -- ^Idle stats
    } deriving (Show)

-- | Interface data taken from @\/proc\/net\/dev@
data NetStat = NetStat
    { _interface :: T.Text
    -- ^ Inteface name (e.g. enp5s0)
    , _downTotal :: Integer
    -- ^ Total bits downloaded
    , _upTotal   :: Integer
    -- ^ Total bits uploaded
    } deriving (Show)

-- | Dynamic state that will be used with the 'ScriptState' constructor
data SystemState = SystemState
    { _cpuState   :: [CpuStat]
    -- ^ List of all 'CpuStat', one for each core and one for the total average
    , _netState   :: [NetStat]
    -- ^ List of all 'NetStat', one for each interface
    , _dbusState  :: Conc.MVar T.Text
    -- ^ Currently playing song
    , _emailState :: Conc.MVar Int
    -- ^ Number of unread emails
    }

-- | Static state that must be set at startup in 'main'
data StaticState = StaticState
    { _uname :: T.Text
    -- ^ Output of @uname -r@
    } deriving (Show)

-- | State of the script
data MyState = MyState
    { _systemState :: SystemState
    -- ^ Dynamic state that will be updated
    , _staticState :: StaticState
    -- ^ Static state that should be set once at the start of the program
    }

makeLenses ''MyState
makeLenses ''StaticState
makeLenses ''SystemState
makeLenses ''NetStat


-- | Specialized 'S.StateT' with 'MyState' and 'T.Text'
type StateM = S.StateT MyState IO T.Text

instance Ord NetStat where
    compare (NetStat _ d1 _) (NetStat _ d2 _) = compare d2 d1

instance Eq NetStat where
    (NetStat _ d1 _) ==  (NetStat _ d2 _) = d1 == d2

instance Default CpuStat where
    def = CpuStat 0 0 0

instance Default NetStat where
    def = NetStat "Empty" 0 0

instance Default StaticState where
    def = StaticState "uname"

instance Default SystemState where
    def = SystemState [def] [def] undefined undefined

instance Default MyState where
    def = MyState def def

-- | The 'ScriptState' constructor takes a ('SystemState' -> 'IO' ('T.Text', 'SystemState')
-- function but we need a function that takes and returns a 'MyState'
liftSystemScript :: (SystemState -> IO (T.Text, SystemState)) -> StateM
liftSystemScript systemScript = do
    state <- S.get
    (output, newS) <- S.liftIO $ systemScript (_systemState state)
    S.put (MyState newS (_staticState state))
    return output

-- | Separator that is used when the 'Separator' constructor is used
sep :: T.Text
sep = " | "

-- Lift Info into the Free Monad
-- | Lift 'Separator'
separator :: Free Info ()
separator = liftF (Separator ())

-- | Lift 'Icon'
icon :: Config.Color -> Path -> Free Info ()
icon color path  = liftF (Icon color path ())

-- | Lift 'Static'
static :: (StaticState -> T.Text) -> Free Info ()
static text = liftF (Static text ())

-- | Lift 'Script'
script :: IO T.Text -> Free Info ()
script x = liftF (Script x ())

-- | Lift 'ScriptState'
scriptState :: (SystemState -> IO (T.Text, SystemState)) -> Free Info ()
scriptState x = liftF (ScriptState x ())

-- |Interpret the Info + Free Monad.
-- This outputs 'IO' ('T.Text', 'MyState').
-- The 'T.Text' should be fed to dzen2.
--
-- The 'MyState' should be used in the next print statement.
printDzen :: Free Info () -> StateM
printDzen (Free (Separator next)) = do
    rest <- printDzen next
    return $ sep <> rest
printDzen (Free (Icon color path next)) = do
    let iconText = wrapIcon path
        wrappedIcon = wrapColor color iconText
    rest <- printDzen next
    return $ wrappedIcon <> rest
printDzen (Free (Static getStatic next)) = do
    rest <- printDzen next
    staticText <- S.gets (getStatic . _staticState)
    return $ staticText <> rest
printDzen (Free (Script ioScript next)) = do
    output <- S.liftIO ioScript
    rest <- printDzen next
    return $ output <> rest
printDzen (Free (ScriptState ioScript next)) = do
    output <- liftSystemScript ioScript
    rest <- printDzen next
    return $ output <> rest
printDzen (Pure _) =  return ""

-- Utility wrappers
-- | Wrap some text in a color
wrapColor :: Config.Color -> T.Text -> T.Text
wrapColor color text = "^fg(" <> color <> ")" <> text <> "^fg()"

-- | Wrap some path so that it will interpreted like an image by dzen
wrapIcon :: T.Text -> T.Text
wrapIcon path = "^i(" <> path <> ") "
