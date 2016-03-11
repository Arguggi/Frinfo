{-# LANGUAGE OverloadedStrings #-}

module Frinfo.DBus where

import qualified Control.Concurrent    as Conc
import           Control.Exception
import           Control.Monad
import qualified Data.List             as L
import           Data.Maybe
import qualified Data.Text             as T
import qualified DBus                  as DBus
import qualified DBus.Client           as DBusC
import           Formatting            (sformat, (%), (%.))
import qualified Formatting.Formatters as Format
import           Safe

-- | Connect to dbus and return a new MVar that will be updated when the song
-- changes
connectToDbus :: Conc.MVar T.Text -> IO ()
connectToDbus mvar = bracketOnError DBusC.connectSession DBusC.disconnect $ \client -> do
    _ <- DBusC.addMatch client matchSpotify (updateSong mvar)
    return ()

-- | Only match the spotify dbus
matchSpotify :: DBusC.MatchRule
matchSpotify = DBusC.matchAny { DBusC.matchPath = Just "/org/mpris/MediaPlayer2" }

-- | Return the callback that will be used when a signal is received using the
-- 'MVar'
updateSong :: Conc.MVar T.Text -> DBus.Signal -> IO ()
updateSong mvar signal = unless (length (DBus.signalBody signal) < 2) $ do
    -- Spotify should send a message in an array of 2 elements, the first element
    -- is useless, the second one has all of the information
    let body = (DBus.signalBody signal) !! 1
        dict = DBus.fromVariant body
        song = songInfo dict
        -- Either save the new song info in the MVar or keep the old text if an
        -- exception occurs or song == Nothing for some reason
    Conc.modifyMVar_ mvar $ \x -> return (fromMaybe x song)

-- | Maximum song info character length
maxSongChars :: Int
maxSongChars = 55

-- | Get the songInfo from the Dictionary that Spotify sent
songInfo :: Maybe DBus.Dictionary -> Maybe T.Text
songInfo justdict = do
    -- Could of pattern matched on Just, but lets use the Maybe monad
    dict <- justdict
    -- The outer Dictionary only has 1 field: "Metadata" that we don't care about
    let metadata = DBus.dictionaryItems dict
        (_, valueV) = head metadata
    -- For some reason fromVarient :: Maybe DBus.Variant is necessary. Probably
    -- since Spotify sends a Dictionary of {String, Variant}, so we need to tell
    -- haskell that the keys are Variants
    values <- DBus.fromVariant valueV :: Maybe DBus.Variant
    itemsD <- DBus.fromVariant values :: Maybe DBus.Dictionary
    -- We finally have all the (key, value) pairs
    -- Let's get the song name
    let items = DBus.dictionaryItems itemsD
    (_, songNameV) <- L.find (correctTuple spotifySongKey) items
    songNameS <- DBus.fromVariant songNameV
    songName <- DBus.fromVariant songNameS
    -- Let's get the artist's name
    (_, artistV) <- L.find (correctTuple spotifyArtistKey) items
    artistA <- DBus.fromVariant artistV
    -- Spotify sends a list of Artists, only use the first
    artist <- headDef "" <$> DBus.fromVariant artistA
    return $ sformat ((Format.fitRight maxSongChars) %. (Format.string % " - " % Format.string)) artist songName

-- | Key that points to the artist's name
spotifyArtistKey :: T.Text
spotifyArtistKey = "xesam:artist"

-- | Key that points to the song name
spotifySongKey :: T.Text
spotifySongKey = "xesam:title"

-- | Used by 'L.Find'. Since the key of the (Variant, Variant) pair has to
-- converted with fromVariant first we can't simply use 'L.Find' on the tuple
correctTuple :: T.Text -> (DBus.Variant, DBus.Variant) -> Bool
correctTuple targetKey (key, _) = targetKey == keyT
    where
        keyT :: T.Text
        keyT = fromMaybe ("" :: T.Text) $ DBus.fromVariant key
