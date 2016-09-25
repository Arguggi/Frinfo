{-# LANGUAGE OverloadedStrings #-}

module Frinfo.MPD where

import qualified Control.Concurrent as Conc
import Data.Map as Map
import qualified Data.Text as T
import Formatting (sformat, (%))
import qualified Formatting.Formatters as Format
import qualified Network.MPD as MPD

-- | Setup the 'Conc.MVar' by first checking if MPD is running and then calls
-- 'startMPDLoop'
connectToMPD :: Conc.MVar T.Text -> IO ()
connectToMPD mvar
             -- Get the playing song when we first start
 = do
    songResponse <- MPD.withMPD MPD.currentSong
    case songResponse
         -- If we are unable to get the song name
         -- we don't have to update the 'Conc.MVar'
          of
        (Left _) -> startMPDLoop mvar Nothing
        (Right song) -> do
            let songInfo = getSongInfo song
            startMPDLoop mvar songInfo

-- | Updates the 'Conc.Mvar' if necessary and then starts the 'loop'
startMPDLoop :: Conc.MVar T.Text -> Maybe T.Text -> IO ()
startMPDLoop mvar text = do
    updateSong mvar text
    loop mvar

-- | IDLE for a 'MPD.PlayerS' update and then update the 'Conc.MVar'
loop :: Conc.MVar T.Text -> IO ()
loop mvar = do
    songResponse <-
        MPD.withMPD $
        do _ <- MPD.idle [MPD.PlayerS]
           MPD.currentSong
    case songResponse
         -- If we can't connect to MPD wait for some time before trying again
          of
        (Left _) -> Conc.threadDelay 1000000 >> loop mvar
        (Right response) -> do
            let songInfo = getSongInfo response
            updateSong mvar songInfo
            loop mvar

-- | If the 'T.Text' is 'Nothing' we don't have to update the 'Conc.Mvar'
updateSong :: Conc.MVar T.Text -> Maybe T.Text -> IO ()
updateSong mvar Nothing =
    Conc.modifyMVar_ mvar $ \_ -> return "Song with invalid metadata"
updateSong mvar (Just text) = Conc.modifyMVar_ mvar $ \_ -> return text

-- | Get the song info from the MPD response
getSongInfo :: Maybe MPD.Song -> Maybe T.Text
getSongInfo info = do
    song <- info
    let tags = MPD.sgTags song
    -- 'MPD.sgTags' is of type 'Map.Map' 'MPD.Metadata' ['MPD.Value']
    songArtist <- MPD.toText . head <$> Map.lookup MPD.Artist tags
    songTitle <- MPD.toText . head <$> Map.lookup MPD.Title tags
    return $ sformat (Format.stext % " - " % Format.stext) songArtist songTitle
