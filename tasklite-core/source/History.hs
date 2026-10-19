{-|
History of added and closed tasks per time period
(similar to a combination of Taskwarrior's `history` and `ghistory` reports)
-}
module History where

import Protolude (
  Bool,
  Double,
  Eq (..),
  Fractional ((/)),
  Int,
  Integer,
  Maybe (..),
  Num ((*), (+), (-)),
  Ord (max, (<), (<=), (>)),
  Show,
  Text,
  div,
  foldr,
  fromEnum,
  fromIntegral,
  fromMaybe,
  mempty,
  mod,
  show,
  ($),
  (&),
  (.),
  (<&>),
  (<>),
  (||),
 )
import Protolude qualified as P

import Control.Applicative ((<|>))
import Data.Hourglass (
  Date (dateDay, dateMonth, dateYear),
  DateTime (dtDate),
  Month (January),
  Period (periodDays, periodMonths, periodYears),
  dateAddPeriod,
  getWeekDay,
 )
import Data.List (zip3)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Prettyprinter (
  Doc,
  PageWidth (AvailablePerLine),
  annotate,
  fill,
  line,
  pageWidth,
  pretty,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Black, Green, Red, Yellow),
 )

import Config (Config)
import FullTask (FullTask (closed_utc, created_utc, state, ulid))
import Task (TaskState (Deletable, Done, Obsolete))
import Utils (
  bgColrDull,
  colr,
  colrDull,
  maybeBold,
  maybeUnderlined,
  parseUtc,
  ulidTextToDateTime,
 )


-- | Time span by which the history is grouped
data HistoryPeriod = Daily | Weekly | Monthly | Annual
  deriving (Show, Eq)


data HistoryRow = HistoryRow
  { period :: Text
  -- ^ Label of the period (e.g. "2024", "2024-03", or "2024-03-04")
  , added :: Integer
  , done :: Integer
  , obsolete :: Integer
  , deletable :: Integer
  }
  deriving (Show, Eq)


emptyHistoryRow :: Text -> HistoryRow
emptyHistoryRow periodTxt = HistoryRow periodTxt 0 0 0 0


netOfRow :: HistoryRow -> Integer
netOfRow row = row.added - row.done - row.obsolete - row.deletable


pad2 :: P.Int -> Text
pad2 = T.justifyRight 2 '0' . show


{-| First day of the period which contains the given date.
| Weeks start on Monday.
-}
periodStart :: HistoryPeriod -> Date -> Date
periodStart historyPeriod date =
  case historyPeriod of
    Annual -> date{dateMonth = January, dateDay = 1}
    Monthly -> date{dateDay = 1}
    Weekly -> do
      let daysSinceMonday = (fromEnum (getWeekDay date) + 6) `mod` 7
      dateAddPeriod date mempty{periodDays = -daysSinceMonday}
    Daily -> date


-- | First day of the period following the period starting at the given date
nextPeriodStart :: HistoryPeriod -> Date -> Date
nextPeriodStart historyPeriod date =
  dateAddPeriod date $ case historyPeriod of
    Annual -> mempty{periodYears = 1}
    Monthly -> mempty{periodMonths = 1}
    Weekly -> mempty{periodDays = 7}
    Daily -> mempty{periodDays = 1}


-- | Label of a period (e.g. "2024", "2024-03", or "2024-03-04")
periodLabel :: HistoryPeriod -> Date -> Text
periodLabel historyPeriod date = do
  let
    yearMonth =
      show (dateYear date) <> "-" <> pad2 (fromEnum (dateMonth date) + 1)

  case historyPeriod of
    Annual -> show $ dateYear date
    Monthly -> yearMonth
    Weekly -> yearMonth <> "-" <> pad2 (dateDay date)
    Daily -> yearMonth <> "-" <> pad2 (dateDay date)


{-| Aggregate the tasks into rows sorted ascending by period.
| Periods without any activity are included as empty rows
| from the first period with activity up to the current period.
-}
getHistoryRows :: HistoryPeriod -> DateTime -> [FullTask] -> [HistoryRow]
getHistoryRows historyPeriod now tasks = do
  let
    upsert dateTime update =
      Map.alter
        (Just . update . fromMaybe (emptyHistoryRow ""))
        (periodStart historyPeriod $ dtDate dateTime)

    addTask task periodMap = do
      let
        createdMb =
          (task.created_utc P.>>= parseUtc) <|> ulidTextToDateTime task.ulid
        closedMb = task.closed_utc P.>>= parseUtc

        withCreated = case createdMb of
          Nothing -> P.identity
          Just created -> upsert created (\r -> r{added = r.added + 1})

        withClosed = case (closedMb, task.state) of
          (Just closed, Just Done) ->
            upsert closed (\r -> r{done = r.done + 1})
          (Just closed, Just Obsolete) ->
            upsert closed (\r -> r{obsolete = r.obsolete + 1})
          (Just closed, Just Deletable) ->
            upsert closed (\r -> r{deletable = r.deletable + 1})
          _ -> P.identity

      periodMap & withCreated & withClosed

    rowMap = foldr addTask Map.empty tasks

  case Map.lookupMin rowMap of
    Nothing -> []
    Just (firstStart, _) -> do
      let
        lastStart =
          P.max
            (P.maybe firstStart P.fst $ Map.lookupMax rowMap)
            (periodStart historyPeriod $ dtDate now)

      P.iterate (nextPeriodStart historyPeriod) firstStart
        & P.takeWhile (<= lastStart)
          <&> \start ->
            (fromMaybe (emptyHistoryRow "") $ Map.lookup start rowMap)
              { period = periodLabel historyPeriod start
              }


-- | Layout of the bar graph
data GraphLayout = GraphLayout
  { closedWidth :: Int
  -- ^ Width of the left side with the closed tasks
  , addedWidth :: Int
  -- ^ Width of the right side with the added tasks
  , charsPerTask :: Double
  }
  deriving (Show, Eq)


{-| Split the available width (excluding the axis) between the two sides
| proportionally to the largest values on each side,
| so that the bars use as much space as possible with the same scale.
-}
getGraphLayout :: Int -> Integer -> Integer -> GraphLayout
getGraphLayout width maxClosed maxAdded = do
  let
    maxSum = maxClosed + maxAdded
    charsPerTask =
      if maxSum <= 0
        then 0
        else fromIntegral width / fromIntegral maxSum
    closedWidth =
      if maxClosed <= 0
        then 0
        else
          -- Keep at least 1 character for the added tasks
          P.min (if maxAdded > 0 then width - 1 else width) $
            max 1 $
              P.round (fromIntegral maxClosed * charsPerTask)

  GraphLayout
    { closedWidth = closedWidth
    , addedWidth = width - closedWidth
    , charsPerTask = charsPerTask
    }


{-| Bar graph diverging from an axis with closed tasks on the left
| and added tasks on the right side.
-}
formatBars :: Config -> GraphLayout -> HistoryRow -> Doc AnsiStyle
formatBars conf layout row = do
  let
    -- Length of a bar representing the value (at least 1 if value > 0)
    scale :: Integer -> Int
    scale value
      | value <= 0 || layout.charsPerTask <= 0 = 0
      | P.otherwise =
          max 1 $ P.round (fromIntegral value * layout.charsPerTask)

    bar colorName char len =
      if len <= 0
        then mempty
        else
          annotate
            (bgColrDull conf colorName <> colrDull conf colorName)
            (pretty $ T.replicate len $ T.singleton char)

    -- Rounding can lead to bars which are 1 character too long
    clamp width len = max 0 $ P.min width len
    -- Use cumulative lengths for stacked bars to prevent rounding drift
    stackedLen = clamp layout.closedWidth . scale
    doneLen = stackedLen row.done
    obsoleteLen = stackedLen (row.done + row.obsolete) - doneLen
    deletableLen =
      stackedLen (row.done + row.obsolete + row.deletable)
        - doneLen
        - obsoleteLen
    closedLen = doneLen + obsoleteLen + deletableLen

  pretty (T.replicate (layout.closedWidth - closedLen) " ")
    <> bar Black '~' deletableLen
    <> bar Yellow '-' obsoleteLen
    <> bar Green 'X' doneLen
    <> "│"
    <> bar Red '+' (clamp layout.addedWidth $ scale row.added)


formatHistory ::
  Config -> HistoryPeriod -> Bool -> [HistoryRow] -> Doc AnsiStyle
formatHistory conf historyPeriod isTruncated rows =
  if P.null rows
    then annotate (colr conf Yellow) "⚠️ No tasks available"
    else do
      let
        headerStyle = maybeBold conf <> maybeUnderlined conf
        periodName = case historyPeriod of
          Annual -> "Year"
          Monthly -> "Month"
          Weekly -> "Week"
          Daily -> "Day"
        periodWidth =
          P.maximum $
            T.length periodName
              : T.length "Average"
              : (rows <&> T.length . (.period))
        numCols :: [(Text, HistoryRow -> Integer)]
        numCols =
          [ ("Added", (.added))
          , ("Done", (.done))
          , ("Obsolete", (.obsolete))
          , ("Deletable", (.deletable))
          , ("Net", netOfRow)
          ]
        numRows = fromIntegral (P.length rows) :: Integer
        -- Rounded average of a column over all listed periods
        average getter =
          (2 * P.sum (rows <&> getter) + numRows) `div` (2 * numRows)
        averageRow =
          (emptyHistoryRow "Average")
            { added = average (.added)
            , done = average (.done)
            , obsolete = average (.obsolete)
            , deletable = average (.deletable)
            }
        averageNet = average netOfRow
        showAverage = numRows > 1
        widths =
          numCols <&> \(name, getter) ->
            P.maximum $
              T.length name
                : (averageNet : (rows <&> getter) <&> T.length . show)
        tableWidth = periodWidth + P.sum (widths <&> (+ 2))
        maxClosed =
          P.maximum $ rows <&> \row -> row.done + row.obsolete + row.deletable
        maxAdded = P.maximum $ rows <&> (.added)

        netStyle net
          | net > 0 = colr conf Red
          | net < 0 = colr conf Green
          | P.otherwise = mempty

        cell width = pretty . T.justifyRight width ' '

        -- Layout of the graph or Nothing if there is not enough space
        graphLayoutMb pw = do
          let
            -- Remaining width without the gap and the axis
            graphWidth = case pw of
              AvailablePerLine lineWidth _
                | lineWidth < 1000 -> lineWidth - tableWidth - 3
              _ -> 60
          if graphWidth < 6
            then Nothing
            else Just $ getGraphLayout graphWidth maxClosed maxAdded

        withGraph content toGraph =
          pageWidth $ \pw ->
            content <> case graphLayoutMb pw of
              Nothing -> mempty
              Just layout -> "  " <> toGraph layout

        formatRow style row net =
          withGraph
            ( annotate style $
                fill periodWidth (pretty row.period)
                  <> P.fold
                    ( zip3 numCols widths (numCols <&> \(_, getter) -> getter row)
                        <&> \((name, _), width, value) ->
                          "  "
                            <> ( if name == "Net"
                                   then annotate (netStyle net) (cell width (show net))
                                   else cell width (show value)
                               )
                    )
            )
            (\layout -> formatBars conf layout row)

        header =
          withGraph
            ( annotate headerStyle (fill periodWidth $ pretty periodName)
                <> P.fold
                  ( P.zip numCols widths <&> \((name, _), width) ->
                      "  " <> annotate headerStyle (cell width name)
                  )
            )
            ( \layout -> do
                let
                  -- Only show labels which fit completely
                  label width name =
                    if T.length name <= width then name else ""

                annotate headerStyle $
                  cell layout.closedWidth (label layout.closedWidth "Closed")
                    <> "│"
                    <> pretty (label layout.addedWidth "Added")
            )

        legend =
          pageWidth $ \pw -> case graphLayoutMb pw of
            Nothing -> mempty
            Just _ ->
              line
                <> annotate headerStyle "Legend:"
                <+> P.mconcat
                  ( P.intersperse
                      "  "
                      [ annotate (colr conf Red) "+" <+> "Added"
                      , annotate (colr conf Green) "X" <+> "Done"
                      , annotate (colr conf Yellow) "-" <+> "Obsolete"
                      , annotate (colr conf Black) "~" <+> "Deletable"
                      ]
                  )

      header
        <> line
        <> vsep (rows <&> \row -> formatRow mempty row (netOfRow row))
        <> ( if showAverage
               then line <> formatRow (maybeBold conf) averageRow averageNet
               else mempty
           )
        <> legend
        <> ( if isTruncated
               then
                 line
                   <> annotate
                     (colr conf Yellow)
                     ( "This list is truncated. "
                         <> "List all by piping into `cat` or `less`."
                     )
               else mempty
           )


{-| Format the history of the tasks
| and only show as many of the most recent periods as fit the available lines
-}
getHistory ::
  Config ->
  HistoryPeriod ->
  DateTime ->
  Maybe Int ->
  [FullTask] ->
  Doc AnsiStyle
getHistory conf historyPeriod now availableLinesMb tasks = do
  let
    rows = getHistoryRows historyPeriod now tasks
    -- Header, average, legend, and truncation note
    numOfExtraLines = 4
    shownRows = case availableLinesMb of
      Nothing -> rows
      Just availableLines ->
        P.reverse $
          P.take (max 1 $ availableLines - numOfExtraLines) $
            P.reverse rows

  formatHistory
    conf
    historyPeriod
    (P.length shownRows < P.length rows)
    shownRows
