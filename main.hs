import qualified VM.Snapshot as S

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)
import Data.Time.Format
import Data.Time.LocalTime (getZonedTime)
import Data.List (intercalate)
import qualified Data.Text as T

main :: IO ()
main = do
  (vmName:restArgs) <- getArgs
  let commandFlags = parseCommandFlags restArgs
  -- TODO: use ReaderT IO
  when (doTake commandFlags) $ do
    snapshotName <- (vmName ++) <$> timestampSuffix <$> getZonedTime -- getCurrentTimeではUTCの時間が表示されてしまった！
    putStrLn $ "Taking snapshot of " ++ vmName ++ snapshotName
    S.take vmName snapshotName

  when (doDelete commandFlags) $ do
    (oldest:_) <- S.list vmName
    putStrLn $ "Deleting snapshot of " ++ vmName ++ T.unpack (S.name oldest)
    S.delete vmName oldest

timestampSuffix :: FormatTime t => t -> String
timestampSuffix = formatTime defaultTimeLocale "-%F-%H-%M"

parseCommandFlags :: [String] -> CommandFlags
parseCommandFlags commands
  | null commands = error "No command specified!"
  | length commands <= 2 = CommandFlags ("take" `elem` commands) ("delete" `elem` commands)
  | otherwise = error $ "Invalid command: " ++ (intercalate " " commands)

data CommandFlags = CommandFlags { doTake :: Bool, doDelete :: Bool }
