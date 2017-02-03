{-# LANGUAGE OverloadedStrings #-}

module Frinfo.INotify where

import qualified Control.Concurrent as Conc
import Control.Exception
import qualified Control.Foldl as F
import Control.Monad
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified Frinfo.Config as Config
import qualified System.INotify as IN
import qualified Turtle as Tur hiding (FilePath, Text)

-- | Boolean blindness is not fun
data Action
    = Add
    | Remove

-- | Use 'IN.addWatch' to watch for 'IN.Create', 'IN.MoveIn', 'IN.Delete' and
-- 'IN.MoveOut' events in all @new\/@ subfolders of 'mailFolder'. Updates the
-- returned 'Conc.MVar' on each callback
watchEmailFolder :: Conc.MVar Int -> IO ()
watchEmailFolder mvar =
    bracketOnError IN.initINotify IN.killINotify $ \notify -> do
        folders <- allNewFolders
        forM_ folders $ \folder -> do
            files <- getTotalFiles folder
            when (files > 0) $ Conc.modifyMVar_ mvar $ \x -> return (x + files)
            IN.addWatch
                notify
                [IN.Create, IN.MoveIn, IN.Delete, IN.MoveOut]
                (toPreludeFp folder)
                (eventLength mvar)

-- | Turtle gives back 'FS.FilePath' but 'System.INotify' uses 'Prelude.FilePath'
-- so we have to convert between the two.
toPreludeFp :: FS.FilePath -> Prelude.FilePath
toPreludeFp = T.unpack . either id id . FS.toText

-- | Partially apply to get the callback used with 'IN.addWatch'
eventLength :: Conc.MVar Int -> IN.Event -> IO ()
eventLength mvar (IN.Created isDir _) = updateMVar mvar Add isDir
eventLength mvar (IN.MovedIn isDir _ _) = updateMVar mvar Add isDir
eventLength mvar (IN.Deleted isDir _) = updateMVar mvar Remove isDir
eventLength mvar (IN.MovedOut isDir _ _) = updateMVar mvar Remove isDir
eventLength _ _ = return ()

-- | update the 'Conc.MVar' by adding 1 or removing 1
updateMVar
    :: Conc.MVar Int -- ^ MVar to update
    -> Action -- ^ 'Add' or 'Remove' the file from the count in the 'Conc.MVar'
    -> Bool -- ^ if the file is a folder we ignore it
    -> IO ()
updateMVar mvar action isDir =
    unless isDir $
    case action of
        Add -> Conc.modifyMVar_ mvar $ \x -> return (x + 1)
        Remove -> Conc.modifyMVar_ mvar $ \x -> return (x - 1)

-- | Find all the subfolders of 'mailFolder' called @new@
allNewFolders :: IO [FS.FilePath]
allNewFolders = Tur.fold (FS.fromText . Tur.lineToText <$> dirStream) F.list
  where
    dirStream =
        Tur.inproc
        -- Turtle has a find function but it was ~30 times slower than find
        -- This takes ~0.02 seconds, find takes ~0.60 on my machine
            "find"
            [Config.mailFolder, "-name", "new", "-type", "d"]
            Tur.empty

-- | Total number of files in a 'FS.FilePath'
getTotalFiles :: FS.FilePath -> IO Int
getTotalFiles folder = Tur.fold (Tur.ls folder) F.length
