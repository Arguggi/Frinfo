{-# LANGUAGE OverloadedStrings #-}

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

data CpuStat = CpuStat User System Idle deriving (Show)

data State = State [CpuStat]

type User = Integer
type System = Integer
type Idle = Integer
type Path = T.Text

defaultStat :: CpuStat
defaultStat = CpuStat 0 0 0

-- Convenient Dzen -> Free
separator :: Free Dzen ()
separator = liftF (Separator ())

icon :: Color -> Path -> Free Dzen ()
icon color path  = liftF (Icon color path ())

static :: T.Text -> Free Dzen ()
static text = liftF (Static text ())

script :: IO T.Text -> Free Dzen ()
script x = liftF (Script x ())

scriptState :: (State -> IO T.Text) -> State -> Free Dzen ()
scriptState x state = liftF (ScriptState x state ())

instance Functor Dzen where
    fmap f (Separator x) = Separator (f x)
    fmap f (Icon color path x) = Icon color path (f x)
    fmap f (Static text x) = Static text (f x)
    fmap f (Script text x) = Script text (f x)
    fmap f (ScriptState text state x) = ScriptState text state (f x)

-- printer for free
printDzen :: Free Dzen () -> IO T.Text
printDzen (Free (Separator next)) = do
    let sep = " | "
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
