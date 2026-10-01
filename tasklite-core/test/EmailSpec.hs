module EmailSpec where

import Protolude (
  Either (Left, Right),
  Maybe (..),
  Text,
  ($),
  (&),
  (<>),
 )
import Protolude qualified as P

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import Email (emailToImportTask, htmlToText)
import ImportTask (ImportTask (..))
import Task (Task (body, metadata, modified_utc, ulid))


-- | Join header and body lines with CRLF
mkEmail :: [Text] -> BSL.ByteString
mkEmail lines =
  lines & T.intercalate "\r\n" & T.encodeUtf8 & BSL.fromStrict


withImportTask :: BSL.ByteString -> (ImportTask -> Expectation) -> Expectation
withImportTask email check =
  case emailToImportTask email of
    Left error -> P.die error
    Right importTask -> check importTask


metadataField :: Text -> ImportTask -> Maybe Value
metadataField key importTask =
  case importTask.task.metadata of
    Just (Object obj) -> KeyMap.lookup (Key.fromText key) obj
    _ -> Nothing


mailboxJson :: Text -> Text -> Value
mailboxJson name email =
  Object $
    KeyMap.fromList [("name", String name), ("email", String email)]


spec :: Spec
spec = do
  describe "emailToImportTask" $ do
    let
      simpleEmail =
        mkEmail
          [ "From: Jane Doe <jane@example.com>"
          , "To: John <john@example.com>, max@example.com"
          , "Cc: Team <team@example.com>"
          , "Subject: Review the contract"
          , "Date: Wed, 30 Sep 2026 10:00:00 +0200"
          , "Message-ID: <abc.123@example.com>"
          , "Keywords: work, legal"
          , ""
          , "Hi,"
          , ""
          , "please review the contract."
          , ""
          ]

    it "uses the subject and the text as body" $ do
      withImportTask simpleEmail $ \importTask -> do
        importTask.task.body
          `shouldBe` "Review the contract\n\nHi,\n\nplease review the contract."
        P.length importTask.notes `shouldBe` 0
        importTask.tags `shouldBe` ["work", "legal"]

    it "stores sender, recipients, and message ID as metadata" $ do
      withImportTask simpleEmail $ \importTask -> do
        metadataField "from" importTask
          `shouldBe` Just (Array $ V.fromList [mailboxJson "Jane Doe" "jane@example.com"])
        metadataField "to" importTask
          `shouldBe` Just
            ( Array $
                V.fromList
                  [ mailboxJson "John" "john@example.com"
                  , mailboxJson "" "max@example.com"
                  ]
            )
        metadataField "cc" importTask
          `shouldBe` Just (Array $ V.fromList [mailboxJson "Team" "team@example.com"])
        metadataField "messageId" importTask
          `shouldBe` Just (String "<abc.123@example.com>")

    it "derives a deterministic ULID from the date and content" $ do
      withImportTask simpleEmail $ \importTask -> do
        importTask.task.modified_utc `shouldBe` "2026-09-30 08:00:00.000"
        -- 2026-09-30T08:00:00Z
        T.take 10 importTask.task.ulid `shouldBe` "01m3rn7q00"
        withImportTask simpleEmail $ \importTask2 ->
          importTask2.task.ulid `shouldBe` importTask.task.ulid

    it "decodes multipart emails and prefers the plain text part" $ do
      let
        email =
          mkEmail
            [ "From: jane@example.com"
            , "Subject: Multipart"
            , "MIME-Version: 1.0"
            , "Content-Type: multipart/alternative; boundary=\"XX\""
            , ""
            , "--XX"
            , "Content-Type: text/plain; charset=utf-8"
            , "Content-Transfer-Encoding: quoted-printable"
            , ""
            , "Gr=C3=BC=C3=9Fe, a long line which is soft-="
            , "wrapped."
            , "--XX"
            , "Content-Type: text/html; charset=utf-8"
            , ""
            , "<p>Gr&uuml;&szlig;e</p>"
            , "--XX--"
            , ""
            ]
      withImportTask email $ \importTask -> do
        importTask.task.body
          `shouldBe` "Multipart\n\nGrüße, a long line which is soft-wrapped."

    it "ignores text attachments" $ do
      let
        email =
          mkEmail
            [ "From: jane@example.com"
            , "Subject: With attachment"
            , "MIME-Version: 1.0"
            , "Content-Type: multipart/mixed; boundary=\"XX\""
            , ""
            , "--XX"
            , "Content-Type: text/plain"
            , ""
            , "See attachment."
            , "--XX"
            , "Content-Type: text/plain; name=\"notes.txt\""
            , "Content-Disposition: attachment; filename=\"notes.txt\""
            , ""
            , "Attached notes"
            , "--XX--"
            , ""
            ]
      withImportTask email $ \importTask ->
        importTask.task.body `shouldBe` "With attachment\n\nSee attachment."

    it "converts HTML-only emails to plain text" $ do
      let
        email =
          mkEmail
            [ "From: jane@example.com"
            , "Subject: HTML only"
            , "MIME-Version: 1.0"
            , "Content-Type: text/html; charset=utf-8"
            , "Content-Transfer-Encoding: base64"
            , ""
            , -- <html><head><style>p { color: red; }</style></head><body>
              -- <p>Hello <b>Jane</b>,</p><ul><li>One</li><li>Two</li></ul>
              -- </body></html>
              "PGh0bWw+PGhlYWQ+PHN0eWxlPnAgeyBjb2xvcjogcmVkOyB9PC9zdHlsZT48L2hlYWQ+PGJvZHk+"
            , "PHA+SGVsbG8gPGI+SmFuZTwvYj4sPC9wPjx1bD48bGk+T25lPC9saT48bGk+VHdvPC9saT48L3Vs"
            , "PjwvYm9keT48L2h0bWw+"
            , ""
            ]
      withImportTask email $ \importTask ->
        importTask.task.body `shouldBe` "HTML only\n\nHello Jane,\n\n- One\n- Two"

    it "decodes encoded words in headers" $ do
      let
        email =
          mkEmail
            [ "From: =?ISO-8859-1?Q?J=F6rg?= <joerg@example.com>"
            , "Subject: =?UTF-8?B?w5xiZXJwcsO8ZnVuZw==?="
            , ""
            , "Text"
            , ""
            ]
      withImportTask email $ \importTask -> do
        importTask.task.body `shouldBe` "Überprüfung\n\nText"
        metadataField "from" importTask
          `shouldBe` Just (Array $ V.fromList [mailboxJson "Jörg" "joerg@example.com"])

    it "decodes Windows-1252 text" $ do
      let
        email =
          mkEmail
            [ "From: jane@example.com"
            , "Subject: Quotes"
            , "Content-Type: text/plain; charset=windows-1252"
            , ""
            , ""
            ]
            <> BSL.pack [0x93, 0x48, 0x69, 0x94, 0x20, 0x80, 0xE4]
      withImportTask email $ \importTask ->
        importTask.task.body `shouldBe` "Quotes\n\n“Hi” €ä"

    it "accepts emails with LF line endings" $ do
      let
        email =
          "From: jane@example.com\nSubject: Unix\n\nLine 1\nLine 2\n"
      withImportTask email $ \importTask -> do
        importTask.task.body `shouldBe` "Unix\n\nLine 1\nLine 2"

    it "uses only the text as body if there is no subject" $ do
      let email = mkEmail ["From: jane@example.com", "", "Call Jane", ""]
      withImportTask email $ \importTask ->
        importTask.task.body `shouldBe` "Call Jane"

    it "uses only the subject as body if there is no text" $ do
      let email = mkEmail ["Subject: Call Jane", "", ""]
      withImportTask email $ \importTask ->
        importTask.task.body `shouldBe` "Call Jane"

  describe "htmlToText" $ do
    it "keeps line breaks and drops hidden elements" $ do
      htmlToText
        "<title>Title</title><script>alert(1)</script>\
        \<div>Line&nbsp;1<br>Line   2</div><p></p><p>Next</p>"
        `shouldBe` "Line 1\nLine 2\n\nNext"
