{-|
Datatype `ImportTask` plus instances and functions
-}
module ImportTask where

import Protolude (
  Alternative ((<|>)),
  Applicative (pure),
  Char,
  Eq ((==)),
  Functor (fmap),
  Generic,
  Hashable (hash),
  Integral (toInteger),
  Maybe (..),
  Num (abs),
  Semigroup ((<>)),
  Show,
  Text,
  asum,
  fromMaybe,
  hush,
  isJust,
  optional,
  show,
  ($),
  (&),
  (.),
  (/=),
  (<&>),
  (=<<),
  (>>=),
  (||),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson (Value)
import Data.Aeson as Aeson (
  FromJSON (parseJSON),
  ToJSON,
  Value (Object, String),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Hourglass (
  DateTime,
  Time (timeFromElapsedP),
  TimeFormatString,
  timePrint,
  toFormat,
 )
import Data.Text qualified as T
import Data.Time.ISO8601.Duration qualified as Iso
import Data.ULID (ULID, ulidFromInteger)
import FullTask qualified
import Note (Note (..))
import Time.System (dateCurrent)
import Task (
  Task (
    Task,
    awake_utc,
    body,
    closed_utc,
    due_utc,
    group_ulid,
    metadata,
    modified_utc,
    priority_adjustment,
    ready_utc,
    recurrence_duration,
    repetition_duration,
    review_utc,
    state,
    ulid,
    user,
    waiting_utc
  ),
  emptyTask,
  textToTaskState,
 )
import Utils (
  emptyUlid,
  parseUlidText,
  parseUtc,
  parseUtcNum,
  setDateTime,
  toUlidTime,
  zeroTime,
  zeroUlidTxt,
 )


data Annotation = Annotation
  { entry :: Text
  , description :: Text
  }
  deriving (Generic, Eq)


instance Hashable Annotation


instance ToJSON Annotation


instance FromJSON Annotation where
  parseJSON = withObject "annotation" $ \o -> do
    entry <- o .: "entry"
    description <- o .: "description"
    pure Annotation{..}


annotationToNote :: Annotation -> Note
annotationToNote annot@Annotation{entry, description} = do
  let
    utc = entry & parseUtc & fromMaybe (timeFromElapsedP 0 :: DateTime)
    ulidGeneratedRes = annot & (hash >>> toInteger >>> abs >>> ulidFromInteger)
    ulidCombined = (ulidGeneratedRes & P.fromRight emptyUlid) `setDateTime` utc

  Note
    { ulid = (T.toLower . show) ulidCombined
    , body = description
    }


textToNote :: DateTime -> Text -> Note
textToNote utc body =
  let
    ulidGeneratedRes = body & (hash >>> toInteger >>> abs >>> ulidFromInteger)
    ulidCombined = (ulidGeneratedRes & P.fromRight emptyUlid) `setDateTime` utc
  in
    Note
      { ulid = (T.toLower . show) ulidCombined
      , body = body
      }


importUtcFormat :: TimeFormatString
importUtcFormat =
  toFormat ("YYYY-MM-DD H:MI:S.ms" :: [Char])


data ImportTask = ImportTask
  { task :: Task
  , notes :: [Note]
  , tags :: [Text]
  }
  deriving (Show)


emptyImportTask :: ImportTask
emptyImportTask =
  ImportTask
    { task = emptyTask
    , notes = []
    , tags = []
    }


-- | Values a suffixed with a prime (') to avoid name collisions
instance FromJSON ImportTask where
  parseJSON = withObject "task" $ \o -> do
    utc <- o .:? "utc"
    entry <- o .:? "entry"
    creation <- o .:? "creation"
    creation_utc <- o .:? "creation_utc"
    creationUtc <- o .:? "creationUtc"
    created <- o .:? "created"
    created_at <- o .:? "created_at"
    createdAt <- o .:? "createdAt"
    created_utc <- o .:? "created_utc"
    createdUtc_ <- o .:? "createdUtc"

    let
      parsedCreatedUtc =
        parseUtc
          =<< ( utc
                  <|> entry
                  <|> creation
                  <|> creation_utc
                  <|> creationUtc
                  <|> created
                  <|> created_at
                  <|> createdAt
                  <|> created_utc
                  <|> createdUtc_
              )
      createdUtc = fromMaybe zeroTime parsedCreatedUtc

    o_title <- o .:? "title"
    o_body <- o .:? "body"
    description <- o .:? "description"
    let body = fromMaybe "" (o_title <|> o_body <|> description)

    o_priority_adjustment <- o .:? "priority_adjustment"
    urgency <- o .:? "urgency"
    priority <- optional (o .: "priority")
    let priority_adjustment = o_priority_adjustment <|> urgency <|> priority

    modified <- o .:? "modified"
    modified_at <- o .:? "modified_at"
    o_modified_utc <- o .:? "modified_utc"
    modification_date <- o .:? "modification_date"
    updated_at <- o .:? "updated_at"
    let
      -- TODO: Parse the fields first and then combine them with `<|>`
      maybeModified =
        modified
          <|> modified_at
          <|> o_modified_utc
          <|> modification_date
          <|> updated_at
      modified_utc =
        maybeModified
          >>= parseUtc
          & fromMaybe createdUtc
          & timePrint importUtcFormat
          & T.pack

    o_state <- o .:? "state"
    status <- o .:? "status"
    let
      state = textToTaskState =<< (o_state <|> status)
      implicitCloseUtcMaybe =
        if isJust state
          then
            maybeModified
              <|> Just (T.pack $ timePrint importUtcFormat createdUtc)
          else Nothing

    o_tags <- o .:? "tags"
    (o_labelsMb :: Maybe [Value]) <- o .:? "labels"
    let labels =
          o_labelsMb
            <&> ( ( \o_labels ->
                      o_labels <&> \case
                        String txt -> Just txt
                        Object obj ->
                          P.flip parseMaybe obj (\o_ -> o_ .:? "name" .!= "")
                        _ -> Nothing
                  )
                    >>> P.catMaybes
                )

    project <- o .:? "project"
    let
      projects = project <&> (: [])
      tags = fromMaybe [] (o_tags <> labels <> projects)

    due <- o .:? "due"
    o_due_utc <- o .:? "due_utc"
    due_on <- o .:? "due_on"
    let
      maybeDue = due <|> o_due_utc <|> due_on
      due_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeDue)

    awake' <- o .:? "awake"
    awake_at' <- o .:? "awake_at"
    sleep' <- o .:? "sleep"
    sleep_utc' <- o .:? "sleep_utc"
    sleep_until' <- o .:? "sleep_until"
    wait' <- o .:? "wait"
    wait_until' <- o .:? "wait_until"
    let
      maybeAwake =
        awake'
          <|> awake_at'
          <|> sleep'
          <|> sleep_utc'
          <|> sleep_until'
          <|> wait'
          <|> wait_until'
      awake_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeAwake)

    ready' <- o .:? "ready"
    ready_since' <- o .:? "ready_since"
    ready_utc' <- o .:? "ready_utc"
    let
      maybeReady = ready' <|> ready_since' <|> ready_utc'
      ready_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeReady)

    review' <- o .:? "review"
    review_at' <- o .:? "review_at"
    review_since' <- o .:? "review_since"
    review_utc' <- o .:? "review_utc"
    let
      maybeReview = review' <|> review_at' <|> review_since' <|> review_utc'
      review_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeReview)

    waiting' <- o .:? "waiting"
    waiting_since' <- o .:? "waiting_since"
    waiting_utc' <- o .:? "waiting_utc"
    let
      maybewaiting = waiting' <|> waiting_since' <|> waiting_utc'
      waiting_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybewaiting)

    closed <- o .:? "closed"
    o_closed_utc <- o .:? "closed_utc"
    closed_at <- o .:? "closed_at"
    closed_on <- o .:? "closed_on"
    end <- o .:? "end"
    o_end_utc <- o .:? "end_utc"
    end_on <- o .:? "end_on"
    let
      maybeClosed =
        closed
          <|> o_closed_utc
          <|> closed_at
          <|> closed_on
          <|> end
          <|> o_end_utc
          <|> end_on
          <|> implicitCloseUtcMaybe
      closed_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeClosed)

    group_ulid <- o .:? "group_ulid"

    let parseIsoDurationMb durTextMb =
          hush $
            fmap (P.decodeUtf8 . Iso.formatDuration) $
              Iso.parseDuration $
                P.encodeUtf8 $
                  fromMaybe "" durTextMb

    repetition_duration' <- o .:? "repetition_duration"
    repeat_duration' <- o .:? "repeat_duration"
    let
      maybeRepetition = repetition_duration' <|> repeat_duration'
      repetition_duration = parseIsoDurationMb maybeRepetition

    recurrence_duration' <- o .:? "recurrence_duration"
    recur_duration' <- o .:? "recur_duration"
    let
      maybeRecurrence = recurrence_duration' <|> recur_duration'
      recurrence_duration = parseIsoDurationMb maybeRecurrence

    o_notes <-
      asum
        [ o .:? "notes" :: Parser (Maybe [Note])
        , do
            notesMb <- o .:? "notes" :: Parser (Maybe [Text])
            pure $ case notesMb of
              Just textNotes -> Just $ textNotes <&> textToNote createdUtc
              Nothing -> Just []
        ]
    annotations <- o .:? "annotations" :: Parser (Maybe [Annotation])

    let
      notes = case (o_notes, annotations) of
        (Nothing, Nothing) -> []
        (Nothing, Just values) -> values <&> annotationToNote
        (Just theNotes, _) -> case parsedCreatedUtc of
          Just crUtc ->
            theNotes
              <&> ( \theNote ->
                      theNote
                        { Note.ulid =
                            theNote.ulid
                              & parseUlidText
                              <&> P.flip setDateTime crUtc
                              <&> show @ULID
                              & fromMaybe theNote.ulid
                              & T.toLower
                        }
                  )
          Nothing -> theNotes

    (o_userMb :: Maybe Value) <- o .:? "user"
    o_author <- o .:? "author"
    let
      o_userNormMb =
        o_userMb >>= \case
          String txt -> Just txt
          Object obj ->
            P.flip parseMaybe obj (\o_ -> o_ .:? "login" .!= "")
          _ -> Nothing
      userMaybe = o_userNormMb <|> o_author
      user = fromMaybe "" userMaybe

    o_metadata <- o .:? "metadata"
    let
      -- Only delete fields with highest priority,
      -- as they would definitely have been used if available
      -- TODO: Check which fields were actually used and delete only them
      --       (Crudly done for title and body)
      metadata =
        o_metadata
          <|> ( ( o
                    & ( case (o_title, o_body) of
                          (Nothing, Just _) -> KeyMap.delete "body"
                          _ -> KeyMap.delete "title"
                      )
                    & KeyMap.delete "utc"
                    & KeyMap.delete "priority_adjustment"
                    & KeyMap.delete "tags"
                    & (if notes /= [] then KeyMap.delete "notes" else P.identity)
                )
                  & ( \val ->
                        if val == KeyMap.empty
                          then
                            Nothing
                          else Just (Object val)
                    )
              )
      tempTask = Task{ulid = "", ..}

    o_ulid <- o .:? "ulid"
    let
      ulidGeneratedRes =
        tempTask & (hash >>> toInteger >>> abs >>> ulidFromInteger)
      ulidCombined =
        (ulidGeneratedRes & P.fromRight emptyUlid) `setDateTime` createdUtc
      ulid =
        T.toLower $
          fromMaybe
            ""
            (o_ulid <|> Just (show ulidCombined))

    -- let showInt = show :: Int -> Text
    -- uuid           <- o .:? "uuid"
    -- -- Map `show` over `Parser` & `Maybe` to convert possible `Int` to `Text`
    -- id             <- (o .:? "id" <|> ((showInt <$>) <$> (o .:? "id")))
    -- let id = (uuid <|> id)

    let finalTask = tempTask{Task.ulid = ulid}

    pure $ ImportTask finalTask notes tags


setMissingFields :: ImportTask -> P.IO ImportTask
setMissingFields importTaskRec = do
  now <- dateCurrent
  let nowUlidTxt = now & toUlidTime & show & T.toLower
  pure $
    importTaskRec
      { task =
          importTaskRec.task
            { Task.ulid =
                if zeroUlidTxt `T.isPrefixOf` importTaskRec.task.ulid
                  then
                    importTaskRec.task.ulid
                      & T.replace zeroUlidTxt nowUlidTxt
                  else importTaskRec.task.ulid
            , Task.modified_utc =
                if importTaskRec.task.modified_utc == ""
                  || importTaskRec.task.modified_utc == "1970-01-01 00:00:00"
                  || importTaskRec.task.modified_utc == "1970-01-01 00:00:00.000"
                  || parseUtc importTaskRec.task.modified_utc == parseUtcNum 0
                  then
                    now
                      & timePrint (toFormat importUtcFormat)
                      & T.pack
                  else show importTaskRec.task.modified_utc
            }
      }


importTaskToFullTask :: ImportTask -> FullTask.FullTask
importTaskToFullTask ImportTask{task, notes, tags} =
  FullTask.FullTask
    { FullTask.ulid = task.ulid
    , FullTask.body = task.body
    , FullTask.modified_utc = task.modified_utc
    , FullTask.awake_utc = task.awake_utc
    , FullTask.ready_utc = task.ready_utc
    , FullTask.waiting_utc = task.waiting_utc
    , FullTask.review_utc = task.review_utc
    , FullTask.due_utc = task.due_utc
    , FullTask.closed_utc = task.closed_utc
    , FullTask.state = task.state
    , FullTask.group_ulid = task.group_ulid
    , FullTask.repetition_duration = task.repetition_duration
    , FullTask.recurrence_duration = task.recurrence_duration
    , FullTask.tags = Just tags
    , FullTask.notes = Just notes
    , FullTask.priority = task.priority_adjustment
    , FullTask.user = task.user
    , FullTask.metadata = task.metadata
    }
