module Lib
  ( inveterate
  ) where

import CommandTurnstyle

import Control.Applicative ((<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Exception (bracket)
import Control.Monad (filterM, fmap, forever, mapM)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (replace)
import Data.Either (isRight)
import Data.HashTable.IO as H
import Data.List (concat, intercalate, null)
import Data.Maybe (Maybe(..), catMaybes, fromMaybe, maybe)
import Data.Text (Text, pack)
import qualified Data.Text
import Filesystem (isDirectory, listDirectory)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (fromText, valid)
import Filesystem.Path.Rules (decode, encode, posix)
import Options (Options(..), runCommand, simpleOption)
import Prelude
  ( Bool(..)
  , Either(..)
  , IO
  , Show(..)
  , String
  , ($)
  , (&&)
  , (*)
  , (**)
  , (++)
  , (.)
  , (<>)
  , (>>=)
  , (||)
  , any
  , const
  , floor
  , not
  , pure
  , putStrLn
  , return
  , show
  )
import System.Exit (ExitCode(..), exitWith)
import qualified System.INotify as INotify
import Text.Printf (printf)
import Text.Regex.TDFA (CompOption(..), ExecOption(..))
import Text.Regex.TDFA.ByteString (Regex, compile, execute)

data Invocation = Invocation
  { optionIgnorePattern :: Maybe String
  , optionSelectPattern :: Maybe String
  , optionSubstitute :: Maybe String
  , optionTerminateCommandOnEvent :: Bool
  }

instance Options Invocation where
  defineOptions =
    pure Invocation <*>
    simpleOption "ignore" Nothing "Filepath regex pattern to ignore." <*>
    simpleOption
      "select"
      Nothing
      "Filepath regex pattern to select for. The command will only be invoked if a filepath satisfies all patterns. By default, all paths are ignored except matches. When used with ignore patterns, it instead exempts file paths from the ignore list." <*>
    simpleOption
      "substitute"
      Nothing
      "A string to substitute in the command with the name of a modified or created file in the watched directory." <*>
    simpleOption
      "terminate"
      False
      "Whether to terminate an earlier invocation of the command with SIGTERM if a new event occurs. (Useful for live-reload; not good for a preprocessor.)"

data Error =
  Error Text

usageMessage :: Text
usageMessage = "Usage: ivt [options] directory-to-watch command-to-run"

inveterate :: IO ()
inveterate =
  runCommand $ \rawInvocation rawArguments -> do
    case parseInvocation rawInvocation rawArguments of
      Left (Error errorMessage) -> printf "%s" errorMessage
      Right parsedInvocation -> inveterateMain parsedInvocation

data ParsedInvocation = ParsedInvocation
  { filePathFilter :: FilePathFilter
  , substitution :: Maybe ByteString
  , arguments :: Arguments
  , terminateCommandOnEvent :: Bool
  }

data WatchSet = WatchSet
  { descriptors :: H.CuckooHashTable ByteString INotify.WatchDescriptor
  , inotify :: INotify.INotify
  , process :: INotify.Event -> Maybe Event
  }

mkWatchSet :: INotify.INotify -> (INotify.Event -> Maybe Event) -> IO WatchSet
mkWatchSet inotify process =
  H.new >>= \descriptors -> return $ WatchSet descriptors inotify process

watchHandler ::
     WatchSet -> ByteString -> (ByteString -> IO ()) -> INotify.Event -> IO ()
watchHandler watchSet@(WatchSet descriptors inotify process) watchDir onChange rawEvent = do
  let path =
        fromMaybe watchDir $
        fmap ((<>) (watchDir <> "/")) $ INotify.eventFilePath rawEvent
  let event = process rawEvent
  case event of
    Just Execute -> onChange path
    Just AddWatch -> addWatch watchSet onChange path
    Just RemoveWatch -> do
      existing <- H.lookup descriptors path
      fromMaybe (pure ()) $ fmap INotify.removeWatch existing
      H.delete descriptors path
    event -> do
      putStrLn $ show event
      return ()

addWatch :: WatchSet -> (ByteString -> IO ()) -> ByteString -> IO ()
addWatch watchSet@(WatchSet descriptors inotify process) onChange path = do
  watch <-
    INotify.addWatch inotify eventsOfInterest path $
    watchHandler watchSet path onChange
  H.insert descriptors path watch

inveterateMain :: ParsedInvocation -> IO ()
inveterateMain (ParsedInvocation {arguments = Arguments {..}, ..}) =
  INotify.withINotify $ \inotify -> do
    let process event = filterEvent filePathFilter event >>= processEvent
    watchSet <- mkWatchSet inotify process
    commandTurnstyle <-
      mkCommandTurnstyle command substitution $
      if terminateCommandOnEvent
        then Single
        else None
    let onChange path = spinCommandTurnstyle commandTurnstyle path
    enumerateWatchPaths path >>= mapM (addWatch watchSet onChange)
    forever $ threadDelay $ (floor (10.0 ** 5.0)) * 60

data Event
  = Execute
  | RemoveWatch
  | AddWatch
  deriving (Show)

processEvent :: INotify.Event -> Maybe Event
processEvent rawEvent =
  case rawEvent of
    INotify.MovedSelf _ -> Just RemoveWatch
    INotify.DeletedSelf -> Just RemoveWatch
    otherEvent -> do
      case otherEvent of
        INotify.MovedIn {isDirectory, ..} ->
          if isDirectory
            then Just AddWatch
            else Just Execute
        INotify.MovedOut {isDirectory, ..} ->
          if isDirectory
            then Just RemoveWatch
            else Just Execute
        INotify.Created {isDirectory, ..} ->
          if isDirectory
            then Just AddWatch
            else Just Execute
        INotify.Modified {isDirectory, ..} ->
          if isDirectory
            then Nothing
            else Just Execute

enumerateWatchPaths :: ByteString -> IO [ByteString]
enumerateWatchPaths root =
  let decodedPath = decode posix root
   in (fmap . fmap) (encode posix) $ enumerateDirectory decodedPath

enumerateDirectory :: FilePath -> IO [FilePath]
enumerateDirectory root = do
  file <- fmap not $ isDirectory root
  if file
    then return []
    else do
      children <- listDirectory root >>= filterM isDirectory
      descendents <- mapM enumerateDirectory children
      return $ root : concat descendents

eventsOfInterest :: [INotify.EventVariety]
eventsOfInterest =
  [ INotify.Create
  , INotify.Delete
  , INotify.Modify
  , INotify.Move
  , INotify.MoveSelf
  , INotify.DeleteSelf
  ]

parseInvocation :: Invocation -> [String] -> Either Error ParsedInvocation
parseInvocation invocation@(Invocation {..}) rawArguments = do
  filePathFilter <- parseFilePathFilter invocation
  arguments <- parseArguments rawArguments
  let substitution = fmap toByteString optionSubstitute
  Right $
    ParsedInvocation
      filePathFilter
      substitution
      arguments
      optionTerminateCommandOnEvent

newtype FilePathFilter =
  FilePathFilter (ByteString -> Maybe ByteString)

filterEvent :: FilePathFilter -> INotify.Event -> Maybe INotify.Event
filterEvent (FilePathFilter filterFilePath) event =
  if INotify.eventSourceIsSelf event
    then Just event
    else fmap (const event) $ INotify.eventFilePath event >>= filterFilePath

parseFilePathFilter :: Invocation -> Either Error FilePathFilter
parseFilePathFilter (Invocation {..}) = do
  ignorePatterns <- mapM compileRegex $ catMaybes [optionIgnorePattern]
  selectPatterns <- mapM compileRegex $ catMaybes [optionSelectPattern]
  Right $
    FilePathFilter $ \filePath ->
      let anyIgnores = not $ null ignorePatterns
          anySelections = not $ null selectPatterns
          notIgnoredSufficient =
            anyIgnores || (not anyIgnores && not anySelections)
          shouldIgnore = any (matchFilePath filePath) ignorePatterns
          shouldSelect = any (matchFilePath filePath) selectPatterns
       in if (notIgnoredSufficient && (not shouldIgnore || shouldSelect)) ||
             shouldSelect
            then Just filePath
            else Nothing

regexCompileOptions =
  CompOption
    { caseSensitive = True
    , multiline = True
    , rightAssoc = True
    , newSyntax = True
    , lastStarGreedy = True
    }

regexExecuteOption = ExecOption {captureGroups = False}

compileRegex :: String -> Either Error Regex
compileRegex expression =
  case compile regexCompileOptions regexExecuteOption $ toByteString expression of
    Left errorMessage -> Left $ Error $ pack errorMessage
    Right regex -> Right regex

matchFilePath :: ByteString -> Regex -> Bool
matchFilePath filePath regexPattern =
  case execute regexPattern filePath of
    Right (Just _) -> True
    _ -> False

data Arguments = Arguments
  { path :: ByteString
  , command :: ByteString
  }

toByteString :: String -> ByteString
toByteString = toStrict . toLazyByteString . stringUtf8

parseArguments :: [String] -> Either Error Arguments
parseArguments (path:command) = do
  _ <- parsePath $ pack path
  Right $ Arguments (toByteString path) $ toByteString $ intercalate " " command
parseArguments _ = Left $ Error $ usageMessage

parsePath :: Text -> Either Error FilePath
parsePath maybePath =
  let candidate = fromText maybePath
   in case valid candidate of
        True -> Right candidate
        False ->
          Left $
          Error $
          Data.Text.concat ["Not a valid file path: ", pack $ show candidate]
