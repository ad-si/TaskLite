module Note where

import Protolude
import Data.Aeson
import qualified Data.Csv as Csv
import Data.Text as T
import Data.ULID


data Note = Note
  { ulid :: Text
  , body :: Text
  } deriving (Generic, Show, Eq)

instance ToJSON Note

instance FromJSON Note where
  parseJSON = withObject "note" $ \o -> do
    o_ulid  <- o .:? "ulid"

    let
      ulidGenerated = (ulidFromInteger . abs . toInteger . hash) o
      ulid = T.toLower $ fromMaybe "" $
        o_ulid <|> (hush $ ulidGenerated <&> show)

    body <- o .: "body"

    pure Note{..}


instance Hashable Note

instance Csv.ToField [Note] where
  toField = encodeUtf8
    . (T.intercalate ",")
    . fmap (\Note {ulid = ulid, body = body} -> ulid <> ": " <> body)
