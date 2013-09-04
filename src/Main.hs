{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main bdo server. Accepts polling requests and such. Possibly the
-- worst code I've ever written. I want a prize.

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
import           Paths_bdo
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
  currentClient <- newMVar Nothing
  let printCurrentClient = do
         cur <- readMVar currentClient
         case cur of
           Nothing -> T.putStrLn "No current client"
           Just (client,link) -> T.putStrLn $ "Current client is: " <> client <> ", updating link: " <> link
      update client link = do
        clients <- readMVar clients
        case lookup client clients of
          Nothing -> T.putStrLn "Unknown client. To see list of clients: clients"
          Just (links,Just h)
            | link `elem` links -> do T.putStrLn "Sending link update ..."
                                      reply h [] link
                                      hClose h
            | otherwise -> T.putStrLn "That link doesn't exist in the page. To see the page's links: clients"
          _ -> T.putStrLn "That client isn't connected right now."
      updateCurrentClient = do client <- readMVar currentClient
                               case client of
                                 Nothing -> hPutStr stderr "No current client!"
                                 Just (client,link) -> update client link
  void $ forkIO $ flip finally (sClose listener) $ forever $ do
    (h,_,_) <- accept listener
    forkIO $ do
      hSetBuffering h NoBuffering
      headers <- getHeaders h
      case headers of
        ["update"] -> do updateCurrentClient
                         hClose h
        [T.words -> [(importURL . T.unpack) -> Just client,(importURL . T.unpack) -> Just link]] -> do
         T.putStrLn "Updating from socket request."
         update (T.pack (exportURL client))
                (T.pack (exportURL link))
         hClose h
        _ -> do
         case requestMethod headers of
           Just (method,url) -> dispatch h clients method url headers
           _ -> do logLn $ "Request ignored: " <> T.pack (show headers)
                   hClose h
  forever $ do
    line <- T.getLine
    case T.words line of
      ["clients"] -> do clients <- readMVar clients
                        mapM_ (\(referer,(links,_)) ->
                         T.putStr (referer <> ":\n" <> T.unlines (map ("  "<>) links)))
                           clients
                        when (null clients) $ T.putStrLn "No clients"
                        printCurrentClient
      ["update",client,link] -> update client link
      ["update"] -> updateCurrentClient
      ["set",client,link] -> do
        clients <- readMVar clients
        case lookup client clients of
          Nothing -> T.putStr "No such client"
          Just{} -> do modifyMVar_ currentClient (const (return (Just (client,link))))
                       printCurrentClient
      _ -> T.putStrLn $ "Unknown command. Commands: clients, update <client> <stylesheet>, set <client> <stylesheet> (sets the current client/stylesheet), update (no args, uses current client)"


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
  js <- getDataFileName "bdo.js" >>= T.readFile
  return $
    T.unlines [js
              ,"bdo.host = " <> T.pack (show ("http://" <> host <> "/" :: Text)) <> ";"
              ,"bdo.init();"
              ]

replyJs h = reply h [("Content-Type","text/javascript; charset=utf-8")]
