module Lib
  ( inveterate
  ) where

import Control.Applicative ((<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Exception (bracket)
import Control.Monad (fmap, forever, mapM)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (replace)
import Data.ByteString.UTF8 (toString)
import Data.Either (isRight)
import Data.List (intercalate, null)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Text (Text, concat, pack)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (fromText, valid)
import Options (Options(..), runCommand, simpleOption)
import Prelude
  ( Bool(..)
  , Either(..)
  , IO
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
  , show
  )
import System.Exit (ExitCode(..), exitWith)
import qualified System.INotify as INotify
import System.Process (callCommand)
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
      "Whether to terminate an earlier invocation of the command if a new event occurs. (Useful for live-reload; not good for a preprocessor.)"

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

inveterateMain :: ParsedInvocation -> IO ()
inveterateMain (ParsedInvocation {arguments = Arguments {..}, ..}) =
  INotify.withINotify $ \inotify -> do
    let eventsOfInterest =
          [ INotify.Create
          , INotify.Delete
          , INotify.Modify
          , INotify.Move
          , INotify.MoveSelf
          , INotify.DeleteSelf
          ]
    let exit = do
          putStrLn $ (show path) ++ " was moved or deleted."
          exitWith $ ExitFailure 127
    let renderCommand filePath =
          toString $
          case substitution of
            Just substitution ->
              toStrict $ replace substitution filePath command
            Nothing -> command
    let handler event = do
          case event of
            INotify.MovedSelf _ -> exit
            INotify.DeletedSelf -> exit
            event -> do
              putStrLn $ show event
              let FilePathFilter filterFilePath = filePathFilter
              case INotify.eventFilePath event >>= filterFilePath of
                Just filePath -> callCommand $ renderCommand filePath
                Nothing -> pure ()
    INotify.addWatch inotify eventsOfInterest path handler
    forever $ threadDelay $ (floor (10.0 ** 5.0)) * 60

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

parseFilePathFilter :: Invocation -> Either Error FilePathFilter
parseFilePathFilter (Invocation {..}) = do
  ignorePatterns <- mapM compileRegex $ catMaybes [optionIgnorePattern]
  selectPatterns <- mapM compileRegex $ catMaybes [optionSelectPattern]
  Right $
    FilePathFilter $ \filePath ->
      let anyIgnores = not $ null ignorePatterns
          shouldIgnore = any (matchFilePath filePath) ignorePatterns
          shouldSelect = any (matchFilePath filePath) selectPatterns
       in if shouldSelect || (not shouldIgnore && anyIgnores)
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
          Error $ concat ["Not a valid file path: ", pack $ show candidate]
