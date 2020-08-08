module SimpleAeson (
  lookupKey,
  lookupKeyEither,
  lookupKey'
  ) where

import Data.Aeson.Types
import Data.Text (Text)

-- | looks up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: k)

-- | like lookupKey but returns error message if not found
lookupKeyEither :: FromJSON a => Text -> Object -> Either String a
lookupKeyEither k = parseEither (.: k)

-- | like lookupKey but raises an error if no key found
lookupKey' :: FromJSON a => Text -> Object -> a
lookupKey' k =
  either error id . lookupKeyEither k
