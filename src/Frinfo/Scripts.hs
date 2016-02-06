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

-- |Get name of the song that is playing
getSong :: IO T.Text
getSong = return "Test Song"

-- |Get list of cpu usage from @\/proc\/stat@
-- If you cpu has 8 cores there will be 9 items: 1 for each thread
-- and 1 for the average
getCpuStat :: IO [CpuStat]
getCpuStat = do
    stat <- filterCpuStats <$> TIO.readFile "/proc/stat"
    case Atto.parseOnly cpuStatParser stat of
        (Left _) -> return [defaultCpuStat]
        (Right x) -> return x

-- | Return a new state with the interface that downloaded the most bits
-- since the last state.
getNetAverage :: SystemState -> IO (T.Text, SystemState)
getNetAverage oldState = do
    let oldNetState = netState oldState
    newState <- getNetStat
    return (netSpeed . headDef defaultNetStat . sort $ zipWith netAverage oldNetState newState, oldState { netState = newState })

-- |Pretty print an Interface name and traffic
netSpeed :: NetStat -> T.Text
netSpeed (NetStat inter up down) = T.pack $ printf "%10s " (T.unpack inter) <> (printf "%5d" up <> "KB/s " <> downIcon) <> (printf "%5d" down <> "KB/s ")
    where downIcon = T.unpack $ wrapColor upColor (wrapIcon "/home/arguggi/dotfiles/icons/xbm8x8/net_down_03.xbm")

-- |Get bits sent and received for every interface from @\/proc\/net\/dev@
getNetStat :: IO [NetStat]
getNetStat = do
    stat <- filterNetStats <$> TIO.readFile "/proc/net/dev"
    return $ fmap parseNet stat

-- |Return a new state with the updated stats for each thread
-- since the last state.
getCpuAverage :: SystemState -> IO (T.Text, SystemState)
getCpuAverage oldState = do
    let oldCpuState = cpuState oldState
    stat <- filterCpuStats <$> TIO.readFile "/proc/stat"
    case Atto.parseOnly cpuStatParser stat of
        (Left _) -> return ("", oldState)
        (Right newState) -> return (padShow $ zipWith cpuAverage oldCpuState newState, oldState {cpuState = newState})

parseNet :: T.Text -> NetStat
parseNet x = getTotal $ T.words x

-- |Parse @\/proc\/net\/dev@ file
getTotal :: [T.Text] -> NetStat
getTotal (interfaceName:upTotalT:_:_:_:_:_:_:_:downTotalT:_) = NetStat interfaceName (read . T.unpack $ upTotalT :: Integer) (read . T.unpack $ downTotalT :: Integer)
getTotal _ = defaultNetStat

-- |Get the time in the local time zone
getTime :: IO T.Text
getTime = do
    time <- getZonedTime
    return . T.pack $ formatTime defaultTimeLocale "%a %e %b %T" time

-- |Pretty print the total uptime
getUptime :: IO T.Text
getUptime = do
    uptime <- TIO.readFile "/proc/uptime"
    case Atto.parseOnly uptimeParser uptime of
        (Left _) -> return ""
        (Right double) -> return $ toUptimeText (round double :: Integer)

-- |Filter the @\/proc\/stat@ file, we only need lines that start with cpu
filterCpuStats :: T.Text -> T.Text
filterCpuStats = T.unlines . filter ("cpu" `T.isPrefixOf`) . T.lines

-- |Filter the @\/proc\/net\/dev@ file, we don't need the @lo@ interface
filterNetStats :: T.Text -> [T.Text]
filterNetStats text = filter isntLo noHeader
    where noHeader = drop 2 $ T.lines text

isntLo :: T.Text -> Bool
isntLo x = first /=  "lo:"
    where (first:_) = T.words x

-- |Get free and used ram from @\/proc\/meminfo@
getRam :: IO T.Text
getRam = do
    memInfo <- (take 3 . T.lines) <$> TIO.readFile "/proc/meminfo"
    let total = headDef "" $ filter ("MemTotal:" `T.isPrefixOf`) memInfo
        available = headDef "" $ filter ("MemAvailable:" `T.isPrefixOf`) memInfo
        totalGb = kbToMb (totalMemKb . T.words $ total)
        availableGb = kbToMb (totalMemKb . T.words $ available)
        freeGb = totalGb - availableGb
    return $ padRam freeGb <> "M / " <> padRam totalGb <> "M"

-- |Pad the ram text so that it's always al least wide 4 characters
padRam :: Integer ->  T.Text
padRam x = T.pack $ printf " %4d" x

-- |Get the cpu fan RPM from @\/sys\/class\/hwmon\/hwmon1\/fan2_input@
getCpuRpm :: IO T.Text
getCpuRpm = do
    rpm <- readFile "/sys/class/hwmon/hwmon1/fan2_input"
    return . T.pack $ printf " %4d RPM" (readDef 0 $ initSafe rpm :: Integer)

-- |Get Megabits from Kilobits
kbToMb :: Integer -> Integer
kbToMb kb = quot kb 1024

-- |Parse the @\/proc\/meminfo@ file
totalMemKb :: [T.Text] -> Integer
totalMemKb (_:total:_:_) = read . T.unpack $ total
totalMemKb _ = 0

toUptimeText :: Integer -> T.Text
toUptimeText totalSecs = padTime days "d" <> padTime hours "h" <> padTime minutes "m" <> padTime seconds "s"
    where (days, remDays) = quotRem totalSecs secDay
          (hours, remHours) = quotRem remDays secHour
          (minutes, seconds) = quotRem remHours secMinute

-- |Pad the time units so they are always wide 2 charaters
padTime :: Integer -> T.Text -> T.Text
padTime x unit = T.pack ( printf " %2d" x) <> unit

-- |Seconds in a day
secDay :: Integer
secDay = 24 * secHour
-- |Seconds in an hour
secHour :: Integer
secHour = 60 * secMinute
-- |Seconds in a minute
secMinute :: Integer
secMinute = 60

-- |Average 2 cpu stats, as seen here:
cpuAverage :: CpuStat -> CpuStat -> Integer
cpuAverage (CpuStat user1 system1 idle1) (CpuStat user2 system2 idle2) =
    quot' ((user2 + system2 - user1 - system1) * 100) (user2 + system2 + idle2 - user1 - system1 - idle1)

-- |Average 2 net stats. Return data in Kilobits
netAverage :: NetStat -> NetStat -> NetStat
netAverage (NetStat _ up1 down1) (NetStat interface2 up2 down2) =
    NetStat interface2 (quot (up2 - up1) 1024) (quot (down2 - down1) 1024)

-- |Safe 'quot' that returns 0 if the denominator is 0
quot' :: Integer -> Integer -> Integer
quot' _ 0 = 0
quot' a b = quot a b

-- |Pad the cpu % so they are always wide 3 characters
padShow :: [Integer] -> T.Text
padShow x = T.pack $ foldl (<>) "" padded
    where padded = map (printf " %3d%%") x
