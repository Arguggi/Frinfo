{-# LANGUAGE OverloadedStrings #-}

module Honky.Parsers where

import qualified Data.Attoparsec.Text as Atto
import           Honky.Free

statParser :: Atto.Parser [CpuStat]
statParser = Atto.many' statParser'

statParser' :: Atto.Parser CpuStat
statParser' = do
    user <- Atto.string "cpu" *> Atto.choice [Atto.digit, Atto.space] *> Atto.space *> Atto.decimal
    system <- Atto.space *> (Atto.decimal :: Atto.Parser Integer) *> Atto.space *> Atto.decimal
    idle <- Atto.space *> Atto.decimal <* Atto.skipWhile (not . Atto.isEndOfLine)
    Atto.endOfLine
    return $ CpuStat user system idle

uptimeParser :: Atto.Parser Double
uptimeParser = Atto.double <* Atto.takeText
