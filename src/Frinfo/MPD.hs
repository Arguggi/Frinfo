{-# LANGUAGE OverloadedStrings #-}

module Frinfo.MPD where

import qualified Control.Concurrent    as Conc
import           Control.Monad
import           Data.Map              as Map
import qualified Data.Text             as T
import           Formatting            (sformat, (%))
import qualified Formatting.Formatters as Format
import qualified Network.MPD           as MPD

-- | Updates the T.Text in the Conc.MVar when MPD changes song
connectToMPD :: Conc.MVar T.Text -> IO Conc.ThreadId
connectToMPD mvar = do
    -- Get the playing song when we first start
    songResponse <- MPD.withMPD MPD.currentSong
    let songInfo = getSongInfo songResponse
    updateSong mvar songInfo
    Conc.forkIO (loop mvar)

-- | IDLE for a 'MPD.PlayerS' update and then update the 'Conc.MVar'
loop :: Conc.MVar T.Text -> IO ()
loop mvar = forever $ do
    songResponse <- MPD.withMPD $ do
        _ <- MPD.idle [MPD.PlayerS]
        MPD.currentSong
    let songInfo = getSongInfo songResponse
    updateSong mvar songInfo

-- | If the 'T.Text' is 'Nothing' we don't have to update the 'Conc.Mvar'
updateSong :: Conc.MVar T.Text -> Maybe T.Text -> IO ()
updateSong _ Nothing = return ()
updateSong mvar (Just text) = Conc.modifyMVar_ mvar $ \_ -> return text

-- | Get the song info from the MPD response
getSongInfo :: MPD.Response (Maybe MPD.Song) -> Maybe T.Text
getSongInfo (Left _) = Nothing
getSongInfo (Right info) = do
    song <- info
    let tags = MPD.sgTags song
    -- 'MPD.sgTags' is of type 'Map.Map' 'MPD.Metadata' ['MPD.Value']
    songArtist <- MPD.toText . head <$> Map.lookup MPD.Artist tags
    songTitle <- MPD.toText . head  <$> Map.lookup MPD.Title tags
    return $ sformat (Format.stext % " - " % Format.stext) songArtist songTitle
