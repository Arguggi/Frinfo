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
import           System.IO
import           Text.Printf

data Dzen next = Separator next | Icon Color Path next | Script (IO T.Text) next | Done
data Player = Spotify | Mpd | None

type Color = T.Text
type Path = T.Text

data CpuStat = CpuStat User System Idle deriving (Show)
type CpuNum = Int
type Average = Integer
type User = Integer
type System = Integer
type Idle = Integer

headphoneColor :: Color
headphoneColor = "#ea8b2a"

secondsDelay :: Int -> Int
secondsDelay x = x * 1000000

separator :: Free Dzen ()
--separator = Free (Separator (Pure ()))
separator = liftF (Separator ())

icon :: Color -> Path -> Free Dzen ()
--icon color path  = Free (Icon color path (Pure ()))
icon color path  = liftF (Icon color path ())

done :: Free Dzen ()
done = Free Done

script :: IO T.Text -> Free Dzen ()
script x = liftF (Script x ())

instance Functor Dzen where
    fmap f (Separator x) = Separator (f x)
    fmap f (Icon color path x) = Icon color path (f x)
    fmap f (Script text x) = Script text (f x)
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
printDzen (Free Done) =  return ""
printDzen (Pure _) =  return ""

printLoop :: IO ()
printLoop = do
    hSetBuffering stdout NoBuffering
    printLoop'

printLoop' :: IO ()
printLoop' = forever $ do
    printDzen freeStruc >>= TIO.putStrLn
    Conc.threadDelay (secondsDelay 1)

freeStruc :: Free Dzen ()
freeStruc = do
    icon headphoneColor "/home/arguggi/dotfiles/icons/xbm8x8/phones.xbm"
    script getSong
    separator
    script getUptime
    separator
    script getTime
    separator
    script getCpuAverage
    separator

wrapColor :: Color -> T.Text -> T.Text
wrapColor color text = "^fg(" <> color <> ") " <> text <> " ^fg()"

wrapIcon :: T.Text -> T.Text
wrapIcon path = "^i(" <> path <> ") "

getTime :: IO T.Text
getTime = do
    time <- getCurrentTime
    return . T.pack $ formatTime defaultTimeLocale "%a %e %b %T" time

getUptime :: IO T.Text
getUptime = do
    uptime <- TIO.readFile "/proc/uptime"
    case Atto.parseOnly uptimeParser uptime of
        (Left _) -> return ""
        (Right double) -> return ((T.pack . show) (round double :: Integer))


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

getCpuAverage :: IO T.Text
getCpuAverage = do
    stat1 <- filterStats <$> TIO.readFile "/proc/stat"
    Conc.threadDelay 10000
    stat2 <- filterStats <$> TIO.readFile "/proc/stat"
    case parseBoth stat1 stat2 of
        (Left _) -> return ""
        (Right (first, second)) -> return . padShow $ zipWith cpuAverage first second

padShow :: [Integer] -> T.Text
padShow x = T.pack $ foldl (<>) "" padded
    where padded = map (printf " %3d%%") x

filterStats :: T.Text -> T.Text
filterStats = T.unlines . filter ("cpu" `T.isPrefixOf`) . T.lines

parseBoth :: T.Text -> T.Text -> Either String ([CpuStat], [CpuStat])
parseBoth stat1 stat2 = do
    cpu1 <- Atto.parseOnly statParser stat1
    cpu2 <- Atto.parseOnly statParser stat2
    return (cpu1, cpu2)

statParser :: Atto.Parser [CpuStat]
statParser = Atto.many' statParser'

statParser' :: Atto.Parser CpuStat
--statParser' = CpuStat <$> (Atto.string "cpu" *> Atto.digit *> Atto.space *> Atto.decimal) <*> (Atto.decimal *> Atto.decimal) <*> (Atto.decimal <* Atto.takeText)
statParser' = do
    user <- Atto.string "cpu" *> Atto.choice [Atto.digit, Atto.space] *> Atto.space *> Atto.decimal
    system <- Atto.space *> (Atto.decimal :: Atto.Parser Integer) *> Atto.space *> Atto.decimal
    idle <- Atto.space *> Atto.decimal <* Atto.skipWhile (not . Atto.isEndOfLine)
    Atto.endOfLine
    return $ CpuStat user system idle
