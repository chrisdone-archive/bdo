{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HTTP utilities.

module Http where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Network.URL
import           Numeric
import           Prelude hiding (catch)
import           System.IO

-- | Parse a POST request's parameters.
parsePost :: Text -> Maybe [(Text,Text)]
parsePost body = fmap (map (T.pack *** T.pack) . url_params)
                      (importURL ("http://x/x?" ++ T.unpack body))

-- | Get the request method.
requestMethod :: [Text] -> Maybe (Text,URL)
requestMethod headers =
   case T.words (T.concat (take 1 headers)) of
     [method,(importURL . T.unpack) -> Just url,_] ->
       return (method,url)
     _ -> Nothing

-- | Get the request body.
requestBody :: [Text] -> Text -> Maybe Text
requestBody headers body = do
  len <- lookup "content-length:" (map (T.break (==' ') . T.map toLower) headers)
  case readDec (T.unpack (T.unwords (T.words len))) of
    [(l,"")] -> return (T.take l body)
    _ -> Nothing

-- | Read up to the headers.
getHeaders :: Handle -> IO [Text]
getHeaders h = go [] where
  go ls = do
    l <- catch (T.hGetLine h)
               (\(e::IOException) -> return "\r")
    if l == "\r"
       then return (reverse ls)
       else go (T.filter (/='\r') l : ls)

-- | Make a HTTP reply.
reply :: Handle -> [(Text,Text)] -> Text -> IO ()
reply h headers body = T.hPutStrLn h resp where
  resp = T.unlines ["HTTP/1.1 200 OK"
                   ,"Content-Length: " <> T.pack (show (T.length body))
                   ,"Access-Control-Allow-Origin: *"
                   ,T.unlines (map (\(key,value) -> key <> ": " <> value) headers)] <>
         body

-- | Lookup the given header from the headers list.
lookupHeader :: Text -> [Text] -> Maybe Text
lookupHeader key headers =
  fmap (T.drop 2 . T.dropWhile (/=':'))
       (lookup key (map (T.break (==':') . T.map toLower) headers))
