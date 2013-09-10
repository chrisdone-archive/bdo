{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main bdo server.

module Main where

import Bdo
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Http
import           Network
import           Network.URL
import           Paths_bdo
import           Prelude hiding (catch)
import           System.Environment
import           System.IO

-- | Main entry point.
main :: IO ()
main = do
  (listenPort:_) <- getArgs
  hSetBuffering stdout NoBuffering
  startServer (read listenPort)
