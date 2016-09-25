module Frinfo.Structure where

import           Control.Monad.Free
import qualified Frinfo.Config              as Config
import Frinfo.Scripts
import Frinfo.Free

-- | Build the data structure that will then be 'interpreted'
-- See 'Info' for the available constructors
freeStruc :: Free Info ()
freeStruc = do
    icon Config.headphoneColor Config.songIcon
    scriptState getSong
    separator
    icon Config.emailColor Config.emailIcon
    scriptState getUnreadEmails
    separator
    script getRam
    separator
    scriptState getNetAverage
    icon Config.upColor Config.upSpeedIcon
    separator
    scriptState getCpuAverage
    --script getCpuRpm
    separator
    icon Config.unameColor Config.unameIcon
    static uname
    separator
    script getUptime
    separator
    script getTime
