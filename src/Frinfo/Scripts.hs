{-# LANGUAGE OverloadedStrings #-}

module Frinfo.Scripts where

import qualified Control.Concurrent    as Conc
import           Control.Lens
import qualified Data.Attoparsec.Text  as Atto
import           Data.Default
import           Data.List             (foldl', sort)
import           Data.Monoid
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import qualified Data.Text.Read        as Read
import           Data.Time.Format      (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime   (getZonedTime)
import           Formatting            (sformat, (%), (%.))
import qualified Formatting.Formatters as Format
import qualified Frinfo.Config         as Config
import           Frinfo.Free
import           Frinfo.Parsers
import           Safe
import           System.IO             as SIO

-- | Get Battery percent level
getBatteryPerc :: IO T.Text
getBatteryPerc =
    SIO.withFile Config.batteryFile ReadMode $ \file -> do
        stat <- Read.decimal <$> TIO.hGetContents file :: IO (Either String (Int, T.Text))
        case stat of
            Left  _ -> return "0"
            Right (y, _) -> return (formatted y <> "%")
        where
            formatted = sformat (Format.left 2 ' ' %. Format.int)

-- | Get name of the song that is playing
getSong :: SystemState -> IO (T.Text, SystemState)
getSong oldState = do
    songInfo <- Conc.tryReadMVar (oldState ^. dbusState)
    case songInfo of
        Just song -> return (song, oldState)
        Nothing   -> return (Config.noSongPlaying, oldState)

-- | Get total unread emails
getUnreadEmails :: SystemState -> IO (T.Text, SystemState)
getUnreadEmails oldState = do
    unreadEmails <- Conc.tryReadMVar (oldState ^. emailState)
    case unreadEmails of
        Just num -> return (unread num, oldState)
        Nothing  -> return (Config.noEmails, oldState)
    where
        unread = sformat (Format.left 2 ' ' %. Format.int)

-- | Return a new state with the interface that downloaded the most bits
-- since the last state.
getNetAverage :: SystemState -> IO (T.Text, SystemState)
getNetAverage oldState = do
    newState <- getNetStat
    return (netSpeed . headDef def . sort $ newAverages newState
           , oldState & netState .~ newState )
    where
        oldNetState = oldState ^. netState
        newAverages = zipWith netAverage oldNetState

-- | Pretty print an Interface name and traffic
netSpeed :: NetStat -> T.Text
netSpeed (NetStat inter up down) = interfaceText <> downSpeed <> downIcon <> upSpeed
    where interfaceText = padText inter 10
          downSpeed = padWithUnit down 5 "KB/s"
          upSpeed = padWithUnit up 5 "KB/s"
          downIcon = wrapColor Config.upColor (wrapIcon Config.downSpeedIcon)

-- | Get bits sent and received for every interface from @\/proc\/net\/dev@
getNetStat :: IO [NetStat]
getNetStat =
    SIO.withFile Config.netStatFile ReadMode $ \file -> do
        stat <- filterNetStats <$> TIO.hGetContents file
        return $ fmap parseNet stat

-- | Return a new state with the updated stats for each thread
-- since the last state.
getCpuAverage :: SystemState -> IO (T.Text, SystemState)
getCpuAverage oldState =
    SIO.withFile Config.cpuStatFile ReadMode $ \file -> do
        stat <- filterCpuStats <$> TIO.hGetContents file
        let oldCpuState = oldState ^. cpuState
        case Atto.parseOnly cpuStatParser stat of
            (Left _) -> return ("", oldState)
            (Right newState) -> return (padCpu $ zipWith cpuAverage oldCpuState newState,
                                        oldState {_cpuState = newState})
-- | Parse a @\/proc\/net\/dev@ line
parseNet :: T.Text -> NetStat
parseNet x = getTotal $ T.words x

-- | Parse @\/proc\/net\/dev@ file
getTotal :: [T.Text] -> NetStat
getTotal (interfaceName:downTotalT:_:_:_:_:_:_:_:upTotalT:_) =
    NetStat interfaceName (textToInteger upTotalT) (textToInteger downTotalT)
getTotal _ = def

-- | Get the time in the local time zone
getTime :: IO T.Text
getTime = do
    time <- getZonedTime
    return . T.pack $ formatTime defaultTimeLocale "%a %e %b %T" time

-- | Pretty print the total uptime from @\/proc\/uptime@
getUptime :: IO T.Text
getUptime =
    SIO.withFile "/proc/uptime" ReadMode $ \file -> do
        uptime <- TIO.hGetContents file
        case Atto.parseOnly uptimeParser uptime of
            (Left _) -> return ""
            (Right double) -> return $ toUptimeText (round double :: Integer)

-- | Filter the @\/proc\/stat@ file, we only need lines that start with cpu
filterCpuStats :: T.Text -> T.Text
filterCpuStats = T.unlines . filter ("cpu" `T.isPrefixOf`) . T.lines

-- | Filter the @\/proc\/net\/dev@ file, we don't need the first 2 lines and the @lo@ interface
{-| @\/proc\/net\/dev@ example:

> Inter-|   Receive                                                |  Transmit
> face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed
> enp5s0: 3994369869 2843661    0   32    0     0          0      5935 193458772 1643417    0    0    0     0       0          0
> enp6s0: 3994369869 2843661    0   32    0     0          0      5935 193458772 1643417    0    0    0     0       0          0
> wal123: 3994369869 2843661    0   32    0     0          0      5935 193458772 1643417    0    0    0     0       0          0
>     lo: 1632501938 1613280    0    0    0     0          0         0 163250193 1613280    0    0    0     0       0          0

-}
filterNetStats :: T.Text -> [T.Text]
filterNetStats text = filter isntLo noHeader
    where noHeader = drop 2 $ T.lines text

-- | Does the Text start with @lo:@ ?
isntLo :: T.Text -> Bool
isntLo x = first /=  "lo:"
    where first = headDef "" $ T.words x

-- | Get free and used ram from @\/proc\/meminfo@
{-| @\/proc\/meminfo@ example:

> MemTotal:        8132260 kB
> MemFree:          781924 kB
> MemAvailable:    5795616 kB
> Buffers:          313352 kB
> Other useless lines

-}
getRam :: IO T.Text
getRam =
    withFile Config.ramStatFile ReadMode $ \file -> do
        memInfo <- (take 3 . T.lines) <$> TIO.hGetContents file
        let total = headDef "" $ filter ("MemTotal:" `T.isPrefixOf`) memInfo
            available = headDef "" $ filter ("MemAvailable:" `T.isPrefixOf`) memInfo
            totalGb = kbToMb (totalMemKb . T.words $ total)
            availableGb = kbToMb (totalMemKb . T.words $ available)
            freeGb = totalGb - availableGb
        return $ padWithUnit freeGb 4 "M" <> "/ " <> padWithUnit totalGb 4 "M"


-- | Get the cpu fan RPM from @\/sys\/class\/hwmon\/hwmon1\/fan2_input@
getCpuRpm :: IO T.Text
getCpuRpm =
    withFile Config.rpmStatFile ReadMode $ \file -> do
        rpm <- TIO.hGetContents file
        let rpmText = readDef 0 $ T.unpack rpm
        return $ padWithUnit rpmText 4 "RPM"

-- | Get Megabits from Kilobits
kbToMb :: Integer -> Integer
kbToMb kb = quot kb 1024

-- | Parse the @\/proc\/meminfo@ file
totalMemKb :: [T.Text] -> Integer
totalMemKb (_:total:_:_) = textToInteger total
totalMemKb _ = 0

-- | Pretty print seconds to @$DAYd $HOURh $MINm $SECs@, every number is padded so it's
-- wide 2 characters
toUptimeText :: Integer -> T.Text
toUptimeText totalSecs = padTime days "d" <> padTime hours "h" <> padTime minutes "m" <> padTime seconds "s"
    where (days, remDays) = quotRem totalSecs secDay
          (hours, remHours) = quotRem remDays secHour
          (minutes, seconds) = quotRem remHours secMinute

-- | Seconds in a day
secDay :: Integer
secDay = 24 * secHour
-- | Seconds in an hour
secHour :: Integer
secHour = 60 * secMinute
-- | Seconds in a minute
secMinute :: Integer
secMinute = 60

-- | Average 2 cpu stats
cpuAverage :: CpuStat -> CpuStat -> Integer
cpuAverage (CpuStat user1 system1 idle1) (CpuStat user2 system2 idle2) =
    quot' ((user2 + system2 - user1 - system1) * 100) (user2 + system2 + idle2 - user1 - system1 - idle1)

-- | Average 2 net stats. Return data in Kilobits
netAverage :: NetStat -> NetStat -> NetStat
netAverage (NetStat _ up1 down1) (NetStat interface2 up2 down2) =
    NetStat interface2 (quot (up2 - up1) 1024) (quot (down2 - down1) 1024)

-- | Read an Integer defaulting to 0 on error
textToInteger :: T.Text -> Integer
textToInteger = readDef 0 . T.unpack

-- | Safe 'quot' that returns 0 if the denominator is 0
quot' :: Integer -> Integer -> Integer
quot' _ 0 = 0
quot' a b = quot a b

-- | Pad 'T.Text' to width with spaces
padText
    :: T.Text
    -> Int
    -> T.Text
padText text width = sformat (Format.left width ' ' %. Format.stext) text

-- | Pad Integer to width and append a Unit separated by a space
padWithUnit :: Integer  -- ^ Number to pad
            -> Int      -- ^ Min width
            -> T.Text   -- ^ Unit
            -> T.Text   -- ^ Final 'T.Text'
padWithUnit x width = sformat ((Format.left width ' ' %. Format.int) % Format.stext % " ") x

-- | Pad the time units so they are always wide 2 charaters
padTime :: Integer -> T.Text -> T.Text
padTime x = padWithUnit x 2

-- | Pad the cpu % so they are always wide 3 characters and concat them
padCpu :: [Integer] -> T.Text
padCpu xs = foldl' (<>) "" padded
    where padded = map (\x -> padWithUnit x 3 "%") xs
