{-# LANGUAGE OverloadedStrings #-}

module Frinfo.Parsers where

import Data.Attoparsec.Text
import Frinfo.Free

-- | Parse all cpu stat lines from @\/proc\/stat@
cpuStatParser :: Parser [CpuStat]
cpuStatParser = many' cpuStatParser'

{-| Example of the lines we need to parse:

> Name User  Useless System Idle These all are useless
> cpu0 45214 5036666 101606 4034 1048 0 4059 0 0 0

@cpuX@ may also not have any number (just @cpu@), so we need 'choice'
-}
-- | Parse a single cpu stat line from @\/proc\/stat@
cpuStatParser' :: Parser CpuStat
cpuStatParser' = do
    parsedUser <- string "cpu" *> skipMany digit *> skipMany space *> decimal
    parsedSystem <- space *> (decimal :: Parser Integer) *> space *> decimal
    parsedIdle <- space *> decimal <* skipWhile (not . isEndOfLine)
    endOfLine
    return $ CpuStat parsedUser parsedSystem parsedIdle

{-| Example of the line we need to parse

> Uptime  Useless
> 5128.87 18111.21

-}
-- | Parse the uptime in seconds from @\/proc\/uptime@
uptimeParser :: Parser Double
uptimeParser = double <* takeText
