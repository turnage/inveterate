-- inveterate - a file watcher
-- Copyright (C) 2020 Payton Turnage
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
