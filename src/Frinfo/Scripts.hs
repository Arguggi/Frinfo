{-# LANGUAGE OverloadedStrings #-}

module Frinfo.Scripts where

import qualified Data.Attoparsec.Text as Atto
import           Data.List            (sort)
import           Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime  (getZonedTime)
import           Frinfo.Colors
import           Frinfo.Free
import           Frinfo.Parsers
import           Safe
import           Text.Printf

getSong :: IO T.Text
getSong = return "Test Song"

getCpuStat :: IO [CpuStat]
getCpuStat = do
    stat <- filterCpuStats <$> TIO.readFile "/proc/stat"
    case Atto.parseOnly cpuStatParser stat of
        (Left _) -> return [defaultCpuStat]
        (Right x) -> return x

getNetAverage :: SystemState -> IO (T.Text, SystemState)
getNetAverage oldState = do
    let oldNetState = netState oldState
    newState <- getNetStat
    return (netSpeed . headDef defaultNetStat . sort $ zipWith netAverage oldNetState newState, oldState { netState = newState })

netSpeed :: NetStat -> T.Text
netSpeed (NetStat inter up down) = T.pack $ printf "%10s " (T.unpack inter) <> (printf "%5d" up <> "KB/s " <> downIcon) <> (printf "%5d" down <> "KB/s ")
    where downIcon = T.unpack $ wrapColor upColor (wrapIcon "/home/arguggi/dotfiles/icons/xbm8x8/net_down_03.xbm")

getNetStat :: IO [NetStat]
getNetStat = do
    stat <- filterNetStats <$> TIO.readFile "/proc/net/dev"
    return $ fmap parseNet stat

getCpuAverage :: SystemState -> IO (T.Text, SystemState)
getCpuAverage oldState = do
    let oldCpuState = cpuState oldState
    stat <- filterCpuStats <$> TIO.readFile "/proc/stat"
    case Atto.parseOnly cpuStatParser stat of
        (Left _) -> return ("", oldState)
        (Right newState) -> return (padShow $ zipWith cpuAverage oldCpuState newState, oldState {cpuState = newState})

parseNet :: T.Text -> NetStat
parseNet x = getTotal $ T.words x

getTotal :: [T.Text] -> NetStat
getTotal (interfaceName:upTotalT:_:_:_:_:_:_:_:downTotalT:_) = NetStat interfaceName (read . T.unpack $ upTotalT :: Integer) (read . T.unpack $ downTotalT :: Integer)
getTotal _ = defaultNetStat

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

filterCpuStats :: T.Text -> T.Text
filterCpuStats = T.unlines . filter ("cpu" `T.isPrefixOf`) . T.lines

filterNetStats :: T.Text -> [T.Text]
filterNetStats text = filter isntLo noHeader
    where noHeader = drop 2 $ T.lines text

isntLo :: T.Text -> Bool
isntLo x = first /=  "lo:"
    where (first:_) = T.words x

getRam :: IO T.Text
getRam = do
    memInfo <- (take 3 . T.lines) <$> TIO.readFile "/proc/meminfo"
    let total = headDef "" $ filter ("MemTotal:" `T.isPrefixOf`) memInfo
        available = headDef "" $ filter ("MemAvailable:" `T.isPrefixOf`) memInfo
        totalGb = kbToMb (totalMemKb . T.words $ total)
        availableGb = kbToMb (totalMemKb . T.words $ available)
        freeGb = totalGb - availableGb
    return $ padRam freeGb <> "M / " <> padRam totalGb <> "M"

padRam :: Integer ->  T.Text
padRam x = T.pack $ printf " %4d" x

getCpuRpm :: IO T.Text
getCpuRpm = do
    rpm <- readFile "/sys/class/hwmon/hwmon1/fan2_input"
    return . T.pack $ printf " %4d RPM" (readDef 0 $ initSafe rpm :: Integer)

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

netAverage :: NetStat -> NetStat -> NetStat
netAverage (NetStat _ up1 down1) (NetStat interface2 up2 down2) =
    NetStat interface2 (quot (up2 - up1) 1024) (quot (down2 - down1) 1024)

quot' :: Integer -> Integer -> Integer
quot' _ 0 = 0
quot' a b = quot a b

padShow :: [Integer] -> T.Text
padShow x = T.pack $ foldl (<>) "" padded
    where padded = map (printf " %3d%%") x
