module Comment where

import Data.Aeson (
  FromJSON (parseJSON),
  Object,
  ToJSON,
  Value (Object, String),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Csv qualified as Csv
import Data.Text as T (Text, intercalate, toLower)
import Data.ULID (ulidFromInteger)
import Database.SQLite.Simple (FromRow, ToRow, field, fromRow, toRow)
import Database.SQLite.Simple.ToField (toField)
import Protolude (
  Alternative ((<|>)),
  Applicative (pure),
  Eq ((==)),
  Functor (fmap),
  Generic,
  Hashable (hash),
  Integral (toInteger),
  Maybe (..),
  Monad ((>>=)),
  Num (abs),
  Semigroup ((<>)),
  Show,
  encodeUtf8,
  fromMaybe,
  hush,
  show,
  ($),
  (.),
  (<$>),
  (<&>),
  (<*>),
 )


data Comment = Comment
  { ulid :: Text
  , body :: Text
  , user :: Text
  -- ^ Author of the comment
  }
  deriving (Generic, Show, Eq)


instance ToRow Comment where
  toRow Comment{ulid = ulid, body = body, user = user} =
    [ toField ulid
    , toField body
    , toField user
    ]


instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field


instance ToJSON Comment


{-| Parse the user (author) of an object.
Supports a `user` field (plain text or an object with a `login` field)
and an `author` field.
-}
parseUser :: Object -> Parser (Maybe Text)
parseUser o = do
  (o_userMb :: Maybe Value) <- o .:? "user"
  o_author <- o .:? "author"
  let
    o_userNormMb =
      o_userMb >>= \case
        String txt -> Just txt
        Object obj ->
          parseMaybe (\o_ -> o_ .:? "login" .!= "") obj
        _ -> Nothing
  pure $ o_userNormMb <|> o_author


instance FromJSON Comment where
  parseJSON = withObject "comment" $ \o -> do
    o_ulid <- o .:? "ulid"

    let
      ulidGenerated = (ulidFromInteger . abs . toInteger . hash) o
      ulid =
        T.toLower $
          fromMaybe "" $
            o_ulid <|> hush (ulidGenerated <&> show)

    body <- o .: "body"
    user <- parseUser o <&> fromMaybe ""

    pure Comment{..}


instance Hashable Comment


instance Csv.ToField [Comment] where
  toField =
    encodeUtf8
      . T.intercalate ","
      . fmap
        ( \Comment{ulid = ulid, body = body, user = user} ->
            ulid
              <> ": "
              <> body
              <> (if user == "" then "" else " (" <> user <> ")")
        )
