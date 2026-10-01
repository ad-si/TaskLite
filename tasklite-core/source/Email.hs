{-|
Convert emails (RFC 5322 / MIME) to tasks
-}
module Email (
  emailToImportTask,
  htmlToText,
)
where

import Protolude (
  Bool (False),
  Char,
  Either (Left),
  Eq ((==)),
  Maybe (..),
  Text,
  Word8,
  catMaybes,
  concatMap,
  elem,
  filter,
  first,
  fromMaybe,
  fromRight,
  hash,
  isJust,
  mapMaybe,
  not,
  otherwise,
  pure,
  toInteger,
  ($),
  (&),
  (&&),
  (.),
  (<&>),
  (<>),
  (<|>),
  (>>=),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Control.Lens (filtered, preview, toListOf, view, _Right)
import Data.Aeson (Value (Array, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.Char (isSpace)
import Data.Hourglass (timePrint, toFormat)
import Data.IMF (
  Address (Group, Single),
  Mailbox (Mailbox),
  headerCC,
  headerDate,
  headerFrom,
  headerMessageID,
  headerSubject,
  headerText,
  headerTo,
  renderMessageID,
 )
import Data.IMF.Text (renderAddressSpec)
import Data.MIME (
  MIMEMessage,
  WireEntity,
  contentType,
  entities,
  isAttachment,
  matchContentType,
  message,
  mime,
  parse,
  transferDecoded',
 )
import Data.MIME.Charset (CharsetLookup, charsetText', defaultCharsets)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as TE
import Data.ULID (ulidFromInteger)
import Data.Vector qualified as V
import Text.HTML.TagSoup (Tag (TagClose, TagOpen, TagText), parseTags)

import ImportTask (
  ImportTask (..),
  emptyImportTask,
  importUtcFormat,
 )
import Task (Task (..), setMetadataField)
import Utils (emptyUlid, setDateTime, zeroTime, zonedTimeToDateTime)


{-| Supports the charsets of 'defaultCharsets' and Windows-1252.
Unknown charsets are decoded leniently as UTF-8.
-}
charsets :: CharsetLookup
charsets name =
  defaultCharsets name
    <|> ( if name `elem` ["windows-1252", "cp1252"]
            then Just decodeWindows1252
            else Nothing
        )
    <|> Just (T.decodeUtf8With TE.lenientDecode)


-- | Bytes 0x80 to 0x9F differ from ISO-8859-1
decodeWindows1252 :: BS.ByteString -> Text
decodeWindows1252 =
  let
    cp1252High :: Text
    cp1252High =
      "€\x81‚ƒ„…†‡ˆ‰Š‹Œ\x8DŽ\x8F\x90‘’“”•–—˜™š›œ\x9DžŸ"

    decodeByte :: Word8 -> Char
    decodeByte byte
      | byte P.>= 0x80 && byte P.<= 0x9F =
          T.index cp1252High (P.fromIntegral byte P.- 0x80)
      | otherwise = P.chr (P.fromIntegral byte)
  in
    T.pack . P.map decodeByte . BS.unpack


isInlineText :: CI.CI BS.ByteString -> WireEntity -> P.Bool
isInlineText subtype entity =
  matchContentType "text" (Just subtype) (view contentType entity)
    && not (isAttachment entity)


entityText :: WireEntity -> Maybe Text
entityText entity = do
  decoded <- preview (transferDecoded' . _Right) entity
  preview (charsetText' charsets . _Right) decoded


-- | Use the plain text parts and fall back to the HTML parts
messageText :: MIMEMessage -> Text
messageText msg =
  let
    textsOfSubtype subtype =
      msg
        & toListOf (entities . filtered (isInlineText subtype))
        & mapMaybe entityText
    texts = case textsOfSubtype "plain" of
      [] -> textsOfSubtype "html" <&> htmlToText
      plainTexts -> plainTexts
  in
    texts
      <&> (T.replace "\r\n" "\n" >>> T.strip)
      & filter (not . T.null)
      & T.intercalate "\n\n"


-- | Convert HTML to readable plain text
htmlToText :: Text -> Text
htmlToText html =
  let
    hiddenElements = ["head", "script", "style", "title"]
    blockElements =
      [ "address"
      , "article"
      , "aside"
      , "blockquote"
      , "br"
      , "dd"
      , "div"
      , "dl"
      , "dt"
      , "footer"
      , "h1"
      , "h2"
      , "h3"
      , "h4"
      , "h5"
      , "h6"
      , "header"
      , "hr"
      , "ol"
      , "p"
      , "pre"
      , "section"
      , "table"
      , "tr"
      , "ul"
      ]

    collapseSpaces :: Text -> Text
    collapseSpaces text =
      text
        & T.split isSpace
        & T.intercalate " "
        & T.splitOn " "
        & filter (not . T.null)
        & T.intercalate " "
        & ( \collapsed ->
              (if startsWithSpace text then " " else "")
                <> collapsed
                <> (if endsWithSpace text && not (T.null collapsed) then " " else "")
          )

    startsWithSpace text = isJust (T.uncons text) && isSpace (T.head text)
    endsWithSpace text = isJust (T.unsnoc text) && isSpace (T.last text)

    render :: [Text] -> [Tag Text] -> [Text]
    render _ [] = []
    render hiddenStack (tag : rest) = case tag of
      TagOpen name _
        | T.toLower name `elem` hiddenElements ->
            render (T.toLower name : hiddenStack) rest
        | not (P.null hiddenStack) -> render hiddenStack rest
        | T.toLower name == "li" -> "\n- " : render hiddenStack rest
        | T.toLower name `elem` blockElements -> "\n" : render hiddenStack rest
        | otherwise -> render hiddenStack rest
      TagClose name
        | Just (T.toLower name) == P.head hiddenStack ->
            render (P.drop 1 hiddenStack) rest
        | not (P.null hiddenStack) -> render hiddenStack rest
        | T.toLower name `elem` blockElements ->
            "\n" : render hiddenStack rest
        | otherwise -> render hiddenStack rest
      TagText text
        | P.null hiddenStack -> collapseSpaces text : render hiddenStack rest
        | otherwise -> render hiddenStack rest
      _ -> render hiddenStack rest

    -- Strip lines and allow at most one consecutive empty line
    normalizeLines :: [Text] -> [Text]
    normalizeLines lines =
      lines
        <&> T.strip
        & P.foldr
          ( \line acc -> case (line, acc) of
              ("", "" : _) -> acc
              _ -> line : acc
          )
          []
  in
    parseTags html
      & render []
      & T.concat
      & T.lines
      & normalizeLines
      & T.unlines
      & T.strip


mailboxToJson :: Mailbox -> Value
mailboxToJson (Mailbox name addrSpec) =
  Object $
    KeyMap.fromList
      [ ("name", String $ fromMaybe "" name)
      , ("email", String $ renderAddressSpec addrSpec)
      ]


addressesToJson :: [Address] -> Value
addressesToJson addresses =
  addresses
    & concatMap
      ( \case
          Single mailbox -> [mailbox]
          Group _ mailboxes -> mailboxes
      )
      <&> mailboxToJson
    & V.fromList
    & Array


{-| The body consists of the subject and the text content,
separated by an empty line.
So the subject is used as the title of the task in list views.
The ULID is derived from the email's content and date,
so importing the same email twice yields the same ULID.
-}
emailToImportTask :: BSL.ByteString -> Either Text ImportTask
emailToImportTask content = do
  let contentStrict = BSL.toStrict content
  msg <- parse (message mime) contentStrict & first T.pack

  let
    nonEmpty text = if T.null text then Nothing else Just text

    subjectMb = view (headerSubject charsets) msg >>= (T.strip >>> nonEmpty)
    textMb = nonEmpty $ messageText msg

    dateMb = view headerDate msg <&> zonedTimeToDateTime
    utc = fromMaybe zeroTime dateMb

    ulid =
      contentStrict
        & hash
        & toInteger
        & P.abs
        & ulidFromInteger
        & fromRight emptyUlid
        & (`setDateTime` utc)
        & P.show
        & T.toLower

    keywords =
      view (headerText charsets "Keywords") msg
        & fromMaybe ""
        & T.splitOn ","
          <&> T.strip
        & filter (not . T.null)

    addMetadata :: (Text, Maybe Value) -> Task -> Task
    addMetadata (key, valueMb) task = case valueMb of
      Nothing -> task
      Just value -> setMetadataField key value task

    nonEmptyAddresses addresses =
      if P.null addresses then Nothing else Just (addressesToJson addresses)

    metadataFields =
      [ ("from", nonEmptyAddresses $ view (headerFrom charsets) msg)
      , ("to", nonEmptyAddresses $ view (headerTo charsets) msg)
      , ("cc", nonEmptyAddresses $ view (headerCC charsets) msg)
      ,
        ( "messageId"
        , view headerMessageID msg
            <&> (renderMessageID >>> T.decodeUtf8With TE.lenientDecode >>> String)
        )
      , ("comments", view (headerText charsets "Comments") msg <&> String)
      ]

  body <- case catMaybes [subjectMb, textMb] of
    [] -> Left "Email has neither a subject nor any text"
    parts -> pure $ T.intercalate "\n\n" parts

  let
    task =
      P.foldr
        addMetadata
        emptyImportTask.task
          { Task.ulid = ulid
          , Task.body = body
          , Task.modified_utc = case dateMb of
              Nothing -> ""
              Just date -> T.pack $ timePrint (toFormat importUtcFormat) date
          }
        metadataFields

  pure $
    emptyImportTask
      { task = task
      , tags = keywords
      , closedUtcWasExplicit = False
      }
