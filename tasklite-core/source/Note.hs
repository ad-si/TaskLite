module Note where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  (.:),
  (.:?),
 )
import Data.Csv qualified as Csv
import Data.Text as T (Text, intercalate, toLower)
import Data.ULID (ulidFromInteger)
import Protolude (
  Alternative ((<|>)),
  Applicative (pure),
  Eq,
  Functor (fmap),
  Generic,
  Hashable (hash),
  Integral (toInteger),
  Num (abs),
  Semigroup ((<>)),
  Show,
  encodeUtf8,
  fromMaybe,
  hush,
  show,
  ($),
  (.),
  (<&>),
 )


data Note = Note
  { ulid :: Text
  , body :: Text
  }
  deriving (Generic, Show, Eq)


instance ToJSON Note


instance FromJSON Note where
  parseJSON = withObject "note" $ \o -> do
    o_ulid <- o .:? "ulid"

    let
      ulidGenerated = (ulidFromInteger . abs . toInteger . hash) o
      ulid =
        T.toLower $
          fromMaybe "" $
            o_ulid <|> hush (ulidGenerated <&> show)

    body <- o .: "body"

    pure Note{..}


instance Hashable Note


instance Csv.ToField [Note] where
  toField =
    encodeUtf8
      . T.intercalate ","
      . fmap (\Note{ulid = ulid, body = body} -> ulid <> ": " <> body)
