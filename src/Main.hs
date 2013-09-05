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
  let printCurrentClient h = do
         cur <- readMVar currentClient
         case cur of
           Nothing -> T.hPutStrLn h "No current client"
           Just (client,link) -> T.hPutStrLn h $ "Current client is: " <> client <> ", updating link: " <> link
      update h client link = do
        clients <- readMVar clients
        case lookup client clients of
          Nothing -> T.hPutStrLn h "Unknown client. To see list of clients: clients"
          Just (links,Just h')
            | link `elem` links -> do T.hPutStrLn h "Sending link update ..."
                                      reply h' [] link
                                      hClose h'
            | otherwise -> T.hPutStrLn h "That link doesn't exist in the page. To see the page's links: clients"
          _ -> T.hPutStrLn h "That client isn't connected right now."
      updateCurrentClient h = do client <- readMVar currentClient
                                 case client of
                                   Nothing -> hPutStrLn h "No current client!"
                                   Just (client,link) -> update h client link
      printClients h = do clients <- readMVar clients
                          mapM_ (\(referer,(links,_)) -> do
                           T.hPutStrLn h (referer <> ":\n" <>
                                          T.intercalate "\n" (map ("  "<>) links) <>
                                          "\n"))
                            clients
                          printCurrentClient h
      setClient h client link = do
        clients <- readMVar clients
        case lookup client clients of
          Nothing -> T.hPutStrLn h "No such client"
          Just{} -> do modifyMVar_ currentClient (const (return (Just (client,link))))
                       printCurrentClient h
  void $ forkIO $ flip finally (sClose listener) $ forever $ do
    (h,_,_) <- accept listener
    let closing m = finally m (hClose h)
    forkIO $ do
      hSetBuffering h NoBuffering
      headers <- getHeaders h
      case headers of
        ["update"] -> closing (updateCurrentClient h)
        [T.words -> ["update",client,link]] -> update h client link
        ["clients"] -> closing $ printClients h
        [T.words -> ["set",client,link]] -> closing $ setClient h client link
        [T.words -> [(importURL . T.unpack) -> Just client,(importURL . T.unpack) -> Just link]] -> closing $ do
         T.putStrLn "Updating from socket request."
         update h (T.pack (exportURL client))
                  (T.pack (exportURL link))
        _ -> do
         case requestMethod headers of
           Just (method,url) -> dispatch h clients method url headers
           _ -> closing $ T.putStrLn $ "Request ignored: " <> T.pack (show headers)

  forever $ do
    line <- T.getLine
    case T.words line of
      ["clients"] -> printClients stdout
      ["update",client,link] -> update stdout client link
      ["update"] -> updateCurrentClient stdout
      ["set",client,link] -> setClient stdout client link
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
