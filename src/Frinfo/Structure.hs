module Frinfo.Structure where

import Control.Monad.Free
import qualified Frinfo.Config as Config
import Frinfo.Free
import Frinfo.Scripts

-- | Build the data structure that will then be 'interpreted'
-- See 'Info' for the available constructors
freeStruc :: Free Info ()
freeStruc = do
    icon Config.headphoneColor Config.songIcon
    staticIO getSong
    separator
    --icon Config.emailColor Config.emailIcon
    staticIO getUnreadEmails
    separator
    script getRam
    separator
    scriptState getNetAverage
    icon Config.upColor Config.upSpeedIcon
    separator
    scriptState getCpuAverage
    script getCpuTemp
    script getCpuRpm
    separator
    icon Config.unameColor Config.unameIcon
    static _uname
    --separator
    --script getUptime
    separator
    script getTime
