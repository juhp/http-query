{-# LANGUAGE CPP #-}

{-|
A small library for querying a Web API.

@
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO as T
import Network.HTTP.Query

main = do
  let api = "http://www.example.com/api/1"
      endpoint = api +/+ "search"
  res <- webAPIQuery endpoint $ makeKey "q" "needle"
  T.putStrLn $
    case lookupKey "results" res of
      Nothing ->
        fromMaybe "search failed" $ lookupKey "error" res
      Just results ->
        lookupKey' "location" results
@
-}

module Network.HTTP.Query (
  Query,
  QueryItem,
  maybeKey,
  makeKey,
  makeItem,
  (+/+),
  webAPIQuery,
  apiQueryURI,
  lookupKey,
  lookupKeyEither,
  lookupKey'
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types
#if !MIN_VERSION_http_conduit(2,3,3)
import Data.ByteString (ByteString)
#endif
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Network.URI

#if !MIN_VERSION_http_conduit(2,3,1)
type Query = [(ByteString, Maybe ByteString)]
#endif
#if !MIN_VERSION_http_conduit(2,3,3)
type QueryItem = (ByteString, Maybe ByteString)
#endif

-- | Maybe create a query key
maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

-- | Make a singleton key-value Query
makeKey :: String -> String -> Query
makeKey k val = [(B.pack k, Just (B.pack val))]

-- | Make a key-value QueryItem
makeItem :: String -> String -> QueryItem
makeItem k val = (B.pack k, Just (B.pack val))

-- | Combine two path segments with a slash
--
-- > "abc" +/+ "def" == "abc/def"
-- > "abc/" +/+ "def" == "abc/def"
-- > "abc" +/+ "/def" == "abc/def"
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = s ++ t
        | head t == '/' = s ++ t
s +/+ t = s ++ '/' : t

-- | Low-level web api query
webAPIQuery :: (MonadIO m, FromJSON a)
            => String -- ^ url of endpoint
            -> Query -- ^ query options
            -> m a -- ^ returned json
webAPIQuery url params =
  case parseURI url of
    Nothing -> error $ "Cannot parse uri: " ++ url
    Just uri ->
      let req = setRequestCheckStatus $
                setRequestQueryString params $
                requestFromURI_ uri
      in getResponseBody <$> httpJSON req

-- | Get the URI for a web query
apiQueryURI :: String -- ^ url of endpoint
            -> Query -- ^ query options
            -> URI
apiQueryURI url params =
  case parseURI url of
    Nothing -> error $ "Cannot parse uri: " ++ url
    Just uri ->
      let req = setRequestQueryString params $
                requestFromURI_ uri
      in getUri req

-- FIXME support "key1.key2" etc
-- | Look up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: k)

-- | Like lookupKey but returns error message if not found
lookupKeyEither :: FromJSON a => Text -> Object -> Either String a
lookupKeyEither k = parseEither (.: k)

-- | Like lookupKey but raises an error if no key found
lookupKey' :: FromJSON a => Text -> Object -> a
lookupKey' k =
  either error id . parseEither (.: k)
