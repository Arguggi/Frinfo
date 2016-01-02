{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Control.Concurrent       as Conc
import qualified Control.Concurrent.Async as Async
import           Control.Monad            (forever)
import           Control.Monad.Free
import qualified Data.Attoparsec.Text     as Atto
import           Data.Monoid
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           Safe
import           System.IO
import           Text.Printf

data Dzen next = Separator next | Icon Color Path next | Script (IO T.Text) next | ScriptState (State -> IO T.Text) State next | Done
data Player = Spotify | Mpd | None
data State = State [CpuStat]

type Color = T.Text
type Path = T.Text

data CpuStat = CpuStat User System Idle deriving (Show)
type CpuNum = Int
type Average = Integer
type User = Integer
type System = Integer
type Idle = Integer

defaultStat :: CpuStat
defaultStat = CpuStat 0 0 0

headphoneColor :: Color
headphoneColor = "#ea8b2a"

ramColor :: Color
ramColor = "#e86f0c"

secondsDelay :: Int -> Int
secondsDelay x = x * 1000000

separator :: Free Dzen ()
separator = liftF (Separator ())

icon :: Color -> Path -> Free Dzen ()
icon color path  = liftF (Icon color path ())

done :: Free Dzen ()
done = Free Done

script :: IO T.Text -> Free Dzen ()
script x = liftF (Script x ())

scriptState :: (State -> IO T.Text) -> State -> Free Dzen ()
scriptState x state = liftF (ScriptState x state ())

instance Functor Dzen where
    fmap f (Separator x) = Separator (f x)
    fmap f (Icon color path x) = Icon color path (f x)
    fmap f (Script text x) = Script text (f x)
    fmap f (ScriptState text state x) = ScriptState text state (f x)
    fmap _ Done = Done

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
printDzen (Free (Script ioScript next)) = do
    (output, rest) <- Async.concurrently ioScript (printDzen next)
    return $ output <> rest
printDzen (Free (ScriptState ioScript state next)) = do
    (output, rest) <- Async.concurrently (ioScript state) (printDzen next)
    return $ output <> rest
printDzen (Free Done) =  return ""
printDzen (Pure _) =  return ""

printLoop :: IO ()
printLoop = do
    hSetBuffering stdout NoBuffering
    printLoop'

printLoop' :: IO ()
printLoop' = forever $ do
    state <- getCpuStat
    Conc.threadDelay (secondsDelay 1)
    printDzen (freeStruc (State state)) >>= TIO.putStrLn

freeStruc :: State -> Free Dzen ()
freeStruc state = do
    icon headphoneColor "/home/arguggi/dotfiles/icons/xbm8x8/phones.xbm"
    script getSong
    separator
    icon ramColor "/home/arguggi/dotfiles/icons/stlarch/mem1.xbm"
    script getRam
    separator
    scriptState getCpuAverage state
    separator
    script getUptime
    separator
    script getTime

wrapColor :: Color -> T.Text -> T.Text
wrapColor color text = "^fg(" <> color <> ") " <> text <> "^fg()"

wrapIcon :: T.Text -> T.Text
wrapIcon path = "^i(" <> path <> ") "

getRam :: IO T.Text
getRam = do
    memInfo <- (take 3 . T.lines) <$> TIO.readFile "/proc/meminfo"
    let total = headDef "" $ filter ("MemTotal:" `T.isPrefixOf`) memInfo
        available = headDef "" $ filter ("MemAvailable:" `T.isPrefixOf`) memInfo
        totalGb = kbToMb (getKb . T.words $ total)
        availableGb = kbToMb (getKb . T.words $ available)
        freeGb = totalGb - availableGb
    return $ (T.pack . show $ freeGb) <> "M / " <> (T.pack . show $ totalGb) <> "M"

kbToGb :: Integer -> Integer
kbToGb kb = quot kb (1024 * 1024)

kbToMb :: Integer -> Integer
kbToMb kb = quot kb 1024

getKb :: [T.Text] -> Integer
getKb (_:total:_:_) = read . T.unpack $ total
getKb _ = 0

getTime :: IO T.Text
getTime = do
    time <- getCurrentTime
    return . T.pack $ formatTime defaultTimeLocale "%a %e %b %T" time

getUptime :: IO T.Text
getUptime = do
    uptime <- TIO.readFile "/proc/uptime"
    case Atto.parseOnly uptimeParser uptime of
        (Left _) -> return ""
        (Right double) -> return $ toUptimeText (round double :: Integer)


toUptimeText :: Integer -> T.Text
toUptimeText totalSecs = padTime days "d" <> padTime hours "h" <> padTime minutes "m" <> padTime seconds "s"
    where (days, remDays) = quotRem totalSecs secDay
          (hours, remHours) = quotRem remDays secHour
          (minutes, seconds) = quotRem remHours secMinute

padTime :: Integer -> T.Text -> T.Text
padTime x unit = T.pack ( printf " %2d" x) <> unit

secDay :: Integer
secDay = 24 * secHour
secHour :: Integer
secHour = 60 * secMinute
secMinute :: Integer
secMinute = 60

getSong :: IO T.Text
getSong = return "Test Song"

uptimeParser :: Atto.Parser Double
uptimeParser = Atto.double <* Atto.takeText

cpuAverage :: CpuStat -> CpuStat -> Integer
cpuAverage (CpuStat user1 system1 idle1) (CpuStat user2 system2 idle2) =
    quot' ((user2 + system2 - user1 - system1) * 100) (user2 + system2 + idle2 - user1 - system1 - idle1)

quot' :: Integer -> Integer -> Integer
quot' _ 0 = 0
quot' a b = quot a b

getCpuStat :: IO [CpuStat]
getCpuStat = do
    stat <- filterStats <$> TIO.readFile "/proc/stat"
    case Atto.parseOnly statParser stat of
        (Left _) -> return [defaultStat]
        (Right x) -> return x

getCpuAverage :: State -> IO T.Text
getCpuAverage (State oldState) = do
    stat <- filterStats <$> TIO.readFile "/proc/stat"
    case Atto.parseOnly statParser stat of
        (Left _) -> return ""
        (Right newState) -> return . padShow $ zipWith cpuAverage oldState newState

padShow :: [Integer] -> T.Text
padShow x = T.pack $ foldl (<>) "" padded
    where padded = map (printf " %3d%%") x

filterStats :: T.Text -> T.Text
filterStats = T.unlines . filter ("cpu" `T.isPrefixOf`) . T.lines

statParser :: Atto.Parser [CpuStat]
statParser = Atto.many' statParser'

statParser' :: Atto.Parser CpuStat
statParser' = do
    user <- Atto.string "cpu" *> Atto.choice [Atto.digit, Atto.space] *> Atto.space *> Atto.decimal
    system <- Atto.space *> (Atto.decimal :: Atto.Parser Integer) *> Atto.space *> Atto.decimal
    idle <- Atto.space *> Atto.decimal <* Atto.skipWhile (not . Atto.isEndOfLine)
    Atto.endOfLine
    return $ CpuStat user system idle
