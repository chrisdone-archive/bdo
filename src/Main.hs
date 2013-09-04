{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main bdo server. Accepts polling requests and such.

module Main where

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
import           Prelude hiding (catch)
import           System.Environment
import           System.IO

-- | Main entry point.
main :: IO ()
main = do
  (listenPort:_) <- getArgs
  hSetBuffering stdout NoBuffering
  clients <- newMVar []
  listener <- listenOn (PortNumber (fromIntegral (read listenPort :: Int)))
  let update client link = do
       clients <- readMVar clients
       case lookup client clients of
         Nothing -> T.putStrLn "Unknown client. To see list of clients: clients"
         Just (links,Just h)
           | link `elem` links -> do T.putStrLn "Sending link update ..."
                                     reply h [] link
                                     hClose h
           | otherwise -> T.putStrLn "That link doesn't exist in the page. To see the page's links: clients"
         _ -> T.putStrLn "That client isn't connected right now."
  void $ forkIO $ flip finally (sClose listener) $ forever $ do
    (h,_,_) <- accept listener
    forkIO $ do
      hSetBuffering h NoBuffering
      headers <- getHeaders h
      case headers of
        [T.words -> [(importURL . T.unpack) -> Just client,(importURL . T.unpack) -> Just link]] -> do
         T.putStrLn "Updating from socket request."
         update (T.pack (exportURL client))
                (T.pack (exportURL link))
        _ -> do
         case requestMethod headers of
           Just (method,url) -> dispatch h clients method url headers
           _ -> logLn $ "Request ignored: " <> T.pack (show headers)
  forever $ do
    line <- T.getLine
    case T.words line of
      ["clients"] -> readMVar clients >>=
                     mapM_ (\(referer,(links,_)) ->
                             T.putStr (referer <> ":\n" <> T.unlines (map ("  "<>) links)))
      ["update",client,link] -> update client link
      _ -> T.putStrLn $ "Unknown command. Commands: clients, update <client> <stylesheet>"



dispatch :: Handle -> MVar [(Text,([Text],Maybe Handle))] -> Text -> URL -> [Text] -> IO ()
dispatch h cs method url headers = do
  logLn $ T.pack (show h) <> ": Client connected."
  go [("bdo",bdo)
     ,("links",links)
     ,("poll",poll)]

  where bdo = do
          getJs (fromMaybe "localhost" (lookupHeader "host" headers)) >>= replyJs h
          hClose h

        poll = modifyClient (\(links,_) -> (links,Just h))

        links = do
          rest <- T.hGetContents h
          case requestBody headers rest of
            Nothing -> return ()
            Just body -> case parsePost body of
              Nothing -> return ()
              Just params -> case lookup "links" params of
                Nothing -> return ()
                Just links -> do modifyClient (\(_links,handle) -> (T.lines links,handle))
                                 logLn $ T.pack (show h) <> ": Links updated."
                                 reply h [] "Links updated."
          hClose h

        go handlers =
          case find ((`isPrefixOf` (url_path url)).fst) handlers of
              Just (_,handle) -> handle
              Nothing -> do
                logLn $ T.pack (show h) <> ": Unhandled request: " <> T.pack (show url)
                hClose h
        referer = fromMaybe "any" $ lookupHeader "referer" headers
        modifyClient f = modifyMVar_ cs (return . modify) where
          modify xs = case lookup referer xs of
                        Nothing -> (referer,f (mempty,Nothing)) : xs
                        Just x -> (referer,f x) : filter ((/=referer).fst) xs

logLn :: Text -> IO ()
logLn = T.hPutStrLn stderr

getJs :: Text -> IO Text
getJs host = do
  js <- T.readFile "bdo.js"
  return $
    T.unlines [js
              ,"bdo.host = " <> T.pack (show ("http://" <> host <> "/" :: Text)) <> ";"
              ,"bdo.init();"
              ]

replyJs h = reply h [("Content-Type","text/javascript; charset=utf-8")]
