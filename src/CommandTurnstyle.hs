module CommandTurnstyle
  ( TurnstyleLimit(..)
  , CommandTurnstyle
  , mkCommandTurnstyle
  , spinCommandTurnstyle
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (replace)
import Data.ByteString.UTF8 (toString)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (Maybe(..), fromMaybe)
import System.Process (ProcessHandle, spawnCommand, terminateProcess)

data TurnstyleLimit
  = Single
  | None
  deriving (Show, Eq)

data CommandTurnstyle = CommandTurnstyle
  { command :: ByteString
  , substitution :: Maybe ByteString
  , processHandle :: IORef (Maybe ProcessHandle)
  , turnstyleLimit :: TurnstyleLimit
  }

mkCommandTurnstyle ::
     ByteString -> Maybe ByteString -> TurnstyleLimit -> IO CommandTurnstyle
mkCommandTurnstyle command substitution turnstyleLimit = do
  processHandle <- newIORef Nothing
  return $ CommandTurnstyle command substitution processHandle turnstyleLimit

spinCommandTurnstyle :: CommandTurnstyle -> ByteString -> IO ()
spinCommandTurnstyle (CommandTurnstyle {..}) path = do
  let renderedCommand =
        case substitution of
          Just substitution -> toStrict $ replace substitution path command
          Nothing -> command
  currentProcessHandle <- readIORef processHandle
  fromMaybe (pure ()) $ fmap terminateProcess currentProcessHandle
  newProcessHandle <- spawnCommand $ toString renderedCommand
  when (turnstyleLimit == Single) $
    modifyIORef processHandle (const $ Just newProcessHandle)
