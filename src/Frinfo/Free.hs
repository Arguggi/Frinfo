{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Frinfo.Free (module Frinfo.Free) where

import           Control.Monad.Free
import qualified Control.Monad.State.Strict as S
import           Data.Monoid
import qualified Data.Text                  as T
import           Frinfo.Colors

data Dzen next =
      Separator next
      -- ^ Add a simple separator, defined as 'sep'
    | Icon Color Path next
      -- ^ Add an icon, with a 'Color' and 'Path'
    | Static (StaticState -> T.Text) next
      -- ^ Add some fixed text. You have to specify the function
      -- used to extract the Text from the StaticState data
    | Script (IO T.Text) next
      -- ^ Add some IO text.
    | ScriptState (SystemState -> IO (T.Text, SystemState)) next
      -- ^ Add some IO text that also needs access to the previous SystemState
    deriving (Functor)

data CpuStat = CpuStat
    { user   :: Integer
    , system :: Integer
    , idle   :: Integer
    } deriving (Show)

data NetStat = NetStat
    { interface :: T.Text
    -- ^Inteface name (e.g. enp5s0)
    , downTotal :: Integer
    -- ^The total bits downloaded
    , upTotal   :: Integer
    -- ^The total bits uploaded
    } deriving (Show)

data SystemState = SystemState
    { cpuState :: [CpuStat]
    , netState :: [NetStat]
    } deriving (Show)

data StaticState = StaticState
    { uname :: T.Text
    } deriving (Show)

-- |State of the script
data MyState = MyState
    { systemState :: SystemState
    -- ^Dynamic state that will be updated
    , staticState :: StaticState
    -- ^Static state that should be set once at the start of the program
    } deriving (Show)

type Path = T.Text
type StateM = S.StateT MyState IO T.Text

instance Ord NetStat where
    compare (NetStat _ d1 _) (NetStat _ d2 _) = compare d2 d1

instance Eq NetStat where
    (NetStat _ d1 _) ==  (NetStat _ d2 _) = d1 == d2


-- |Default script state
defaultMyState :: MyState
defaultMyState = MyState (SystemState [defaultCpuStat] [defaultNetStat]) (StaticState "uname")

-- |Default Cpu stat
defaultCpuStat :: CpuStat
defaultCpuStat = CpuStat 0 0 0

-- |Default Network stat
defaultNetStat :: NetStat
defaultNetStat = NetStat "Empty" 0 0

-- |The 'ScriptState' constructor takes a ('SystemState' -> 'IO' ('T.Text', 'SystemState')
-- function but we need a function that takes and returns a 'MyState'
liftSystemScript :: (SystemState -> IO (T.Text, SystemState)) -> StateM
liftSystemScript systemScript = do
    state <- S.get
    (output, newS) <- S.liftIO $ systemScript (systemState state)
    S.put (MyState newS (staticState state))
    return output

-- |Separator that is used with the 'Separator' constructor is used
sep :: T.Text
sep = " | "

-- Lift Dzen into the Free Monad
-- |Lift 'Separator'
separator :: Free Dzen ()
separator = liftF (Separator ())

-- |Lift 'Icon'
icon :: Color -> Path -> Free Dzen ()
icon color path  = liftF (Icon color path ())

-- |Lift 'Static'
static :: (StaticState -> T.Text) -> Free Dzen ()
static text = liftF (Static text ())

-- |Lift 'Script'
script :: IO T.Text -> Free Dzen ()
script x = liftF (Script x ())

-- |Lift 'ScriptState'
scriptState :: (SystemState -> IO (T.Text, SystemState)) -> Free Dzen ()
scriptState x = liftF (ScriptState x ())


-- |Interpret the Free Monad.
-- This outputs 'IO' ('T.Text', 'MyState').
-- The 'T.Text' should be fed to dzen2
-- The 'MyState' should be used in the next print statement
printDzen :: Free Dzen () -> StateM
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
    staticText <- S.gets (getStatic . staticState)
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
-- |Wrap some text in a color
wrapColor :: Color -> T.Text -> T.Text
wrapColor color text = "^fg(" <> color <> ")" <> text <> "^fg()"

-- |Wrap some path so that it will interpreted like an image by dzen
wrapIcon :: T.Text -> T.Text
wrapIcon path = "^i(" <> path <> ") "
