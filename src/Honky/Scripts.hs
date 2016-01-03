{-# LANGUAGE OverloadedStrings #-}

module Honky.Scripts where

import qualified Data.Attoparsec.Text as Atto
import           Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime  (getZonedTime)
import           Honky.Free
import           Honky.Parsers
import           Safe
import           Text.Printf


getSong :: IO T.Text
getSong = return "Test Song"

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

getTime :: IO T.Text
getTime = do
    time <- getZonedTime
    return . T.pack $ formatTime defaultTimeLocale "%a %e %b %T" time

getUptime :: IO T.Text
getUptime = do
    uptime <- TIO.readFile "/proc/uptime"
    case Atto.parseOnly uptimeParser uptime of
        (Left _) -> return ""
        (Right double) -> return $ toUptimeText (round double :: Integer)

filterStats :: T.Text -> T.Text
filterStats = T.unlines . filter ("cpu" `T.isPrefixOf`) . T.lines

getRam :: IO T.Text
getRam = do
    memInfo <- (take 3 . T.lines) <$> TIO.readFile "/proc/meminfo"
    let total = headDef "" $ filter ("MemTotal:" `T.isPrefixOf`) memInfo
        available = headDef "" $ filter ("MemAvailable:" `T.isPrefixOf`) memInfo
        totalGb = kbToMb (totalMemKb . T.words $ total)
        availableGb = kbToMb (totalMemKb . T.words $ available)
        freeGb = totalGb - availableGb
    return $ (T.pack . show $ freeGb) <> "M / " <> (T.pack . show $ totalGb) <> "M"

getCpuRpm :: IO T.Text
getCpuRpm = do
    rpm <- readFile "/sys/class/hwmon/hwmon1/fan2_input"
    return . T.pack $ printf " %4d RPM" (readDef 0 $ initSafe rpm :: Int)

kbToMb :: Integer -> Integer
kbToMb kb = quot kb 1024

totalMemKb :: [T.Text] -> Integer
totalMemKb (_:total:_:_) = read . T.unpack $ total
totalMemKb _ = 0

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

cpuAverage :: CpuStat -> CpuStat -> Integer
cpuAverage (CpuStat user1 system1 idle1) (CpuStat user2 system2 idle2) =
    quot' ((user2 + system2 - user1 - system1) * 100) (user2 + system2 + idle2 - user1 - system1 - idle1)

quot' :: Integer -> Integer -> Integer
quot' _ 0 = 0
quot' a b = quot a b

padShow :: [Integer] -> T.Text
padShow x = T.pack $ foldl (<>) "" padded
    where padded = map (printf " %3d%%") x
