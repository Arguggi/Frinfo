{-# LANGUAGE OverloadedStrings #-}

module Frinfo.Config where

import qualified Data.Text as T

type Color = T.Text

headphoneColor :: Color
headphoneColor = "#ea8b2a"

emailColor :: Color
emailColor = "#ffffff"

ramColor :: Color
ramColor = "#e86f0c"

cpuColor :: Color
cpuColor = "#487ff7"

clockColor :: Color
clockColor = "#ffbd55"

unameColor :: Color
unameColor = "#6eadd8"

uptimeColor :: Color
uptimeColor = "#05aa8b"

upColor :: Color
upColor = "#cc0000"

songIcon :: T.Text
songIcon = "/home/ruggero/.config/icons/xbm8x8/phones.xbm"

emailIcon :: T.Text
emailIcon = "/home/ruggero/.config/icons/stlarch/mail7.xbm"

upSpeedIcon :: T.Text
upSpeedIcon = "/home/ruggero/.config/icons/xbm8x8/net_up_03.xbm"

downSpeedIcon :: T.Text
downSpeedIcon = "/home/ruggero/.config/icons/xbm8x8/net_down_03.xbm"

unameIcon :: T.Text
unameIcon = "/home/ruggero/.config/icons/xbm8x8/arch_10x10.xbm"

-- | Default text for an empty MVar in 'Frinfo.Scripts.getSong'
noSongPlaying :: T.Text
noSongPlaying = "No Song Playing"

-- | Default text for an empty MVar in 'Frinfo.Scripts.getUnreadEmails'
noEmails :: T.Text
noEmails = " 0"

-- | Folder where offlineimap saves all the emails
mailFolder :: T.Text
mailFolder = "/home/ruggero/Mail/"

-- | Network interface stats file
netStatFile :: String
netStatFile = "/proc/net/dev"

-- | Cpu activity stats file
cpuStatFile :: String
cpuStatFile = "/proc/stat"

-- | Ram stats file
ramStatFile :: String
ramStatFile = "/proc/meminfo"

-- | Cpu fan rpm file
rpmStatFile :: String
rpmStatFile = "/sys/class/hwmon/hwmon2/fan1_input"

-- | Log exceptions to this file
crashFileName :: String
crashFileName = "frinfo.log"

-- | Battery file
batteryFile :: String
batteryFile = "/sys/class/power_supply/BAT0/capacity"

-- | CPU Temp file
cpuTempFile :: String
--cpuTempFile = "/sys/class/thermal/thermal_zone0/temp"
cpuTempFile = "/sys/class/hwmon/hwmon0/temp1_input"
--cpuTempFile = "/sys/bus/platform/devices/it87.656/hwmon/hwmon0/temp1_input"
