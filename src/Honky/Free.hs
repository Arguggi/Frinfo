{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Honky.Free where


import           Control.Monad.Free
import qualified Control.Monad.State.Strict as S
import           Data.Monoid
import qualified Data.Text                  as T
import           Honky.Colors


data Dzen next =
      Separator next
    | Icon Color Path next
    | Static (StaticState -> T.Text) next
    | Script (IO T.Text) next
    | ScriptState (SystemState -> IO (T.Text, SystemState)) next
    deriving (Functor)

data CpuStat = CpuStat
    { user   :: Integer
    , system :: Integer
    , idle   :: Integer
    } deriving (Show)

data NetStat = NetStat
    { interface :: T.Text
    , downTotal :: Integer
    , upTotal   :: Integer
    } deriving (Show)

data SystemState = SystemState
    { cpuState :: [CpuStat]
    , netState :: [NetStat]
    } deriving (Show)

data StaticState = StaticState
    { uname :: T.Text
    } deriving (Show)

data MyState = MyState
    { systemState :: SystemState
    , staticState :: StaticState
    } deriving (Show)

type Path = T.Text
type StateM = S.StateT MyState IO T.Text

instance Ord NetStat where
    compare (NetStat _ d1 _) (NetStat _ d2 _) = compare d2 d1

instance Eq NetStat where
    (NetStat _ d1 _) ==  (NetStat _ d2 _) = d1 == d2


-- Default empty states
defaultMyState :: MyState
defaultMyState = MyState (SystemState [defaultCpuStat] [defaultNetStat]) (StaticState "uname")

defaultCpuStat :: CpuStat
defaultCpuStat = CpuStat 0 0 0

defaultNetStat :: NetStat
defaultNetStat = NetStat "Empty" 0 0

liftSystemScript :: (SystemState -> IO (T.Text, SystemState)) -> StateM
liftSystemScript systemScript = do
    state <- S.get
    (output, newS) <- S.liftIO $ systemScript (systemState state)
    S.put (MyState newS (staticState state))
    return output

sep :: T.Text
sep = " | "

-- Lift Dzen into the Free Monad
separator :: Free Dzen ()
separator = liftF (Separator ())

icon :: Color -> Path -> Free Dzen ()
icon color path  = liftF (Icon color path ())

static :: (StaticState -> T.Text) -> Free Dzen ()
static text = liftF (Static text ())

script :: IO T.Text -> Free Dzen ()
script x = liftF (Script x ())

scriptState :: (SystemState -> IO (T.Text, SystemState)) -> Free Dzen ()
scriptState x = liftF (ScriptState x ())


-- Interpret the Free Monad
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
wrapColor :: Color -> T.Text -> T.Text
wrapColor color text = "^fg(" <> color <> ")" <> text <> "^fg()"

wrapIcon :: T.Text -> T.Text
wrapIcon path = "^i(" <> path <> ") "
