{-# LANGUAGE OverloadedStrings #-}

module Frinfo.Parsers where

import qualified Data.Attoparsec.Text as Atto
import           Frinfo.Free

-- | Parse all cpu stat lines from @\/proc\/stat@
cpuStatParser :: Atto.Parser [CpuStat]
cpuStatParser = Atto.many' cpuStatParser'

{-| Example of the lines we need to parse:

> Name User  Useless System Idle These all are useless
> cpu0 45214 5036666 101606 4034 1048 0 4059 0 0 0

@cpuX@ may also not have any number (just @cpu@), so we need 'Atto.choice'
-}
-- | Parse a single cpu stat line from @\/proc\/stat@
cpuStatParser' :: Atto.Parser CpuStat
cpuStatParser' = do
    parsedUser <- Atto.string "cpu" *> Atto.choice [Atto.digit, Atto.space] *> Atto.space *> Atto.decimal
    parsedSystem <- Atto.space *> (Atto.decimal :: Atto.Parser Integer) *> Atto.space *> Atto.decimal
    parsedIdle <- Atto.space *> Atto.decimal <* Atto.skipWhile (not . Atto.isEndOfLine)
    Atto.endOfLine
    return $ CpuStat parsedUser parsedSystem parsedIdle

{-| Example of the line we need to parse

> Uptime  Useless
> 5128.87 18111.21

-}
-- | Parse the uptime in seconds from @\/proc\/uptime@
uptimeParser :: Atto.Parser Double
uptimeParser = Atto.double <* Atto.takeText
