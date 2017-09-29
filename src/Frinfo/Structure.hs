{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Frinfo.Structure
    ( freeStruc
    ) where

import Control.Monad.Free
import Control.Monad.Free.Church (F)
import qualified Frinfo.Config as Config
import qualified Data.Text.Lazy.Builder as TLB
import Frinfo.Free
import Frinfo.Scripts

freeStruc :: F Info TLB.Builder
freeStruc = do
    freeStruc'
    return ""

-- | Build the data structure that will then be 'interpreted'
-- See 'Info' for the available constructors
freeStruc' :: (MonadFree Info m) => m ()
freeStruc' = do
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
