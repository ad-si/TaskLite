module Note where

import Protolude
import Data.Aeson
import qualified Data.Csv as Csv
import Data.Text as T

data Note = Note
  { ulid :: Text
  , body :: Text
  } deriving (Generic, Show)

instance ToJSON Note

instance FromJSON Note where
  parseJSON = withObject "note" $ \o -> do
    ulid <- o .: "ulid"
    body <- o .: "body"
    pure Note{..}

instance Hashable Note

instance Csv.ToField [Note] where
  toField = encodeUtf8
    . (T.intercalate ",")
    . fmap (\Note {ulid = ulid, body = body} -> ulid <> ": " <> body)
