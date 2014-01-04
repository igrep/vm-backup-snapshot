{-# LANGUAGE OverloadedStrings #-}

module VM.Snapshot
( Snapshot(..)
, list
, delete
, take
)	where

import System.Process
import System.Exit
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT
import Data.Attoparsec.Text ((.*>))
import Control.Applicative
import Control.Exception
import Prelude hiding (take)

data Snapshot = Snapshot
  { name :: T.Text
  , identifier :: T.Text
  , isCurrent :: Bool } deriving (Show, Read)

list :: String -> IO [Snapshot]
list vmName =
  parseOutput <$> T.pack <$> (readProcess "VBoxManage" ["snapshot", vmName, "list", "--machinereadable"] "")

delete :: String -> Snapshot -> IO ()
delete vmName snapshot = do
  rawSystem "VBoxManage" ["snapshot", vmName, "delete", T.unpack (identifier snapshot)]
    >>= handleExitCode

take :: String -> String -> IO ()
take vmName snapshotName = do
  rawSystem "VBoxManage" ["snapshot", vmName, "take", snapshotName]
    >>= handleExitCode

handleExitCode :: ExitCode -> IO ()
handleExitCode ExitSuccess = return ()
handleExitCode e@(ExitFailure _) = throwIO e

parseOutput :: T.Text -> [Snapshot]
parseOutput = either (const []) id . AT.parseOnly snapshotList

-- NOTE: Definitely bad modeling: a vm's snapshots should be represented as a tree!
-- TODO: Decide whether to include the current snapshot or not.
snapshotList :: AT.Parser [Snapshot]
snapshotList = AT.many1 (currentSnapshot <|> singleSnapshot)

singleSnapshot :: AT.Parser Snapshot
singleSnapshot = do
  (_, sName) <- keyValueLine
  (_, uuid)  <- keyValueLine
  return $ Snapshot sName uuid False

currentSnapshot :: AT.Parser Snapshot
currentSnapshot = do
  (_, sName) <- "Current" .*> keyValueLine
  (_, uuid)  <- "Current" .*> keyValueLine
  "Current" .*> keyValueLine
  return $ Snapshot sName uuid True

keyValueLine :: AT.Parser (T.Text, T.Text)
keyValueLine = do
  key <- AT.takeTill ('=' ==)
  AT.take 1
  value <- (AT.char '"') *> AT.takeTill ('"' ==)
  AT.take 1
  AT.endOfLine
  return (key, value)
