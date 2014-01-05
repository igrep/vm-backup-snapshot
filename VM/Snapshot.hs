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

{-
VBoxManage snapshot <VM_NAME> list --machinereadable の出力例
    SnapshotName="hoge"
    SnapshotUUID="11111111-2222-3333-4444-555555555555"
    SnapshotName-1="foo"
    SnapshotUUID-1="66666666-7777-8888-9999-aaaaaaaaaaaa"
    CurrentSnapshotName="foo"
    CurrentSnapshotUUID="66666666-7777-8888-9999-aaaaaaaaaaaa"
    CurrentSnapshotNode="SnapshotName-1"
をパースする。
-}
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

-- parseOnlyでないと、many1の結果が必ず Partial _ になってしまうらしいため、そうしている。
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
  (_, sName) <- "Current" .*> keyValueLine -- "Current" で始まるkeyValueLine。
  (_, uuid)  <- "Current" .*> keyValueLine
  "Current" .*> keyValueLine
  return $ Snapshot sName uuid True

-- 「Key="Value"」のような内容の行。
-- VBoxManage snapshot <VM_NAME> list --machinereadable の出力結果を構成する各行を表す。
keyValueLine :: AT.Parser (T.Text, T.Text)
keyValueLine = do
  key <- AT.takeTill ('=' ==)
  AT.take 1 -- consume '='
  value <- (AT.char '"') *> AT.takeTill ('"' ==)
  AT.take 1 -- consume the last '"'
  AT.endOfLine
  return (key, value)
