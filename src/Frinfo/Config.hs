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
songIcon = "/home/arguggi/dotfiles/icons/xbm8x8/phones.xbm"

emailIcon :: T.Text
emailIcon = "/home/arguggi/dotfiles/icons/stlarch/mail7.xbm"

upSpeedIcon :: T.Text
upSpeedIcon = "/home/arguggi/dotfiles/icons/xbm8x8/net_up_03.xbm"

downSpeedIcon :: T.Text
downSpeedIcon = "/home/arguggi/dotfiles/icons/xbm8x8/net_down_03.xbm"

unameIcon :: T.Text
unameIcon = "/home/arguggi/dotfiles/icons/xbm8x8/arch_10x10.xbm"

-- | Default text for an empty MVar in 'Frinfo.Scripts.getSong'
noSongPlaying :: T.Text
noSongPlaying = "No Song Playing"

-- | Default text for an empty MVar in 'Frinfo.Scripts.getUnreadEmails'
noEmails :: T.Text
noEmails = " 0"

-- | Folder where offlineimap saves all the emails
mailFolder :: T.Text
mailFolder = "/home/arguggi/Mail/"

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
rpmStatFile = "/sys/class/hwmon/hwmon1/fan2_input"

-- | Log exceptions to this file
crashFile :: String
crashFile = "/home/arguggi/frinfo.log"

-- | Battery file
batteryFile :: String
batteryFile = "/sys/class/power_supply/BAT0/capacity"
