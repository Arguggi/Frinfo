{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Honky.Free where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Free
import           Data.Monoid
import qualified Data.Text                as T
import           Honky.Colors

data Dzen next =
      Separator next
    | Icon Color Path next
    | Static T.Text next
    | Script (IO T.Text) next
    | ScriptState (State -> IO T.Text) State next
    deriving (Functor)

data CpuStat = CpuStat
    { user :: Integer
    , system :: Integer
    , idle :: Integer
    } deriving (Show)
data NetStat = NetStat
    { interface :: T.Text
    , downTotal :: Integer
    , upTotal :: Integer
    } deriving (Show)

data State = State
    { cpuState :: [CpuStat]
    , netState :: [NetStat]
    } deriving (Show)

instance Ord NetStat where
    compare (NetStat _ d1 _) (NetStat _ d2 _) = compare d2 d1

instance Eq NetStat where
    (NetStat _ d1 _) ==  (NetStat _ d2 _) = d1 == d2

type Path = T.Text

defaultCpuStat :: CpuStat
defaultCpuStat = CpuStat 0 0 0

defaultNetStat :: NetStat
defaultNetStat = NetStat "Empty" 0 0

-- Convenient Dzen -> Free
separator :: Free Dzen ()
separator = liftF (Separator ())

sep :: T.Text
sep = " | "

icon :: Color -> Path -> Free Dzen ()
icon color path  = liftF (Icon color path ())

static :: T.Text -> Free Dzen ()
static text = liftF (Static text ())

script :: IO T.Text -> Free Dzen ()
script x = liftF (Script x ())

scriptState :: (State -> IO T.Text) -> State -> Free Dzen ()
scriptState x state = liftF (ScriptState x state ())

printDzen :: Free Dzen () -> IO T.Text
printDzen (Free (Separator next)) = do
    rest <- printDzen next
    return $ sep <> rest
printDzen (Free (Icon color path next)) = do
    let iconText = wrapIcon path
        wrappedIcon = wrapColor color iconText
    rest <- printDzen next
    return $ wrappedIcon <> rest
printDzen (Free (Static text next)) = do
    rest <- printDzen next
    return $ text <> rest
printDzen (Free (Script ioScript next)) = do
    (output, rest) <- Async.concurrently ioScript (printDzen next)
    return $ output <> rest
printDzen (Free (ScriptState ioScript state next)) = do
    (output, rest) <- Async.concurrently (ioScript state) (printDzen next)
    return $ output <> rest
printDzen (Pure _) =  return ""

wrapColor :: Color -> T.Text -> T.Text
wrapColor color text = "^fg(" <> color <> ")" <> text <> "^fg()"

wrapIcon :: T.Text -> T.Text
wrapIcon path = "^i(" <> path <> ") "
