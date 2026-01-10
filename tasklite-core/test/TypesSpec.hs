{-# LANGUAGE QuasiQuotes #-}

module TypesSpec where

import Protolude (Maybe (..), Text, ($), (&), (<&>), (<>))
import Protolude qualified as P

import Test.Hspec (Spec, describe, it, shouldBe)

import Config (defaultConfig)
import Data.Text qualified as T
import Data.Yaml qualified
import FullTask (FullTask (body, notes, tags, ulid), emptyFullTask)
import Lib (insertNotes, insertRecord, insertTags)
import NeatInterpolation (trimming)
import Note (Note (Note, body, ulid))
import Task (Task (body, ulid), emptyTask, taskToEditableMarkdown)
import TestUtils (withMemoryDb)


sampleNotes :: [Note]
sampleNotes =
  [ Note
      { Note.ulid = "01hw5n9m99papg470w8j9k9vd3"
      , Note.body =
          "Sample note 1 is quite long \
          \so we can observer how automatic wrapping \
          \can produce unexpected results."
      }
  , Note
      { Note.ulid = "01hw5n9y3q27zys83b139s7e2r"
      , Note.body =
          "\nSample note 2 is short\n\
          \but has  \nsurprising  \nline breaks."
      }
  ]


spec :: Spec
spec = do
  describe "Task" $ do
    let
      sampleTask =
        emptyTask
          { Task.ulid = "01hs68z7mdg4ktpxbv0yfafznq"
          , Task.body = "Sample task"
          }

    it "can be converted to YAML" $ do
      let
        taskYaml = sampleTask & Data.Yaml.encode & P.decodeUtf8

        expected :: Text
        expected =
          [trimming|
              awake_utc: null
              body: Sample task
              closed_utc: null
              due_utc: null
              group_ulid: null
              metadata: null
              modified_utc: ''
              priority_adjustment: null
              ready_utc: null
              recurrence_duration: null
              repetition_duration: null
              review_utc: null
              state: null
              ulid: 01hs68z7mdg4ktpxbv0yfafznq
              user: ''
              waiting_utc: null
            |]
            <> "\n"

      taskYaml `shouldBe` expected

    it "can be converted to YAML for editing" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        Lib.insertRecord "tasks" memConn sampleTask
        let tags = [0 .. 9] <&> \(i :: P.Int) -> "tag-" <> P.show i
        tagWarnings <- Lib.insertTags defaultConfig memConn Nothing sampleTask tags
        P.show tagWarnings `shouldBe` T.empty

        noteWarnings <-
          Lib.insertNotes defaultConfig memConn Nothing sampleTask sampleNotes
        P.show noteWarnings `shouldBe` T.empty

        taskYaml <- taskToEditableMarkdown memConn sampleTask
        let
          expected :: P.ByteString
          expected =
            [trimming|
                ---
                awake_utc: null
                closed_utc: null
                due_utc: null
                group_ulid: null
                metadata: null
                modified_utc: ''
                priority_adjustment: null
                ready_utc: null
                recurrence_duration: null
                repetition_duration: null
                review_utc: null
                state: null
                ulid: 01hs68z7mdg4ktpxbv0yfafznq
                user: ''
                waiting_utc: null

                # | Existing tags and notes can't be edited here, but new ones can be added

                # tags: ["tag-0","tag-1","tag-2","tag-3","tag-4","tag-5","tag-6","tag-7","tag-8","tag-9"]
                tags: []

                # notes:
                # - Sample note 1 is quite long so we can observer how automatic wrapping can produce unexpected results.
                # - Sample note 2 is short
                #     but has
                #     surprising
                #     line breaks.
                notes: []
                ...

                Sample task
              |]
              & P.encodeUtf8

        taskYaml `shouldBe` expected

  describe "FullTask" $ do
    let
      sampleFullTask =
        emptyFullTask
          { FullTask.ulid = "01hs68z7mdg4ktpxbv0yfafznq"
          , FullTask.body = "Sample task"
          , FullTask.tags = Just ["tag1", "tag2"]
          , FullTask.notes = Just sampleNotes
          }

    it "can be converted to YAML" $ do
      let
        taskYaml = sampleFullTask & Data.Yaml.encode & P.decodeUtf8

        expected :: Text
        expected =
          [trimming|
              awake_utc: null
              body: Sample task
              closed_utc: null
              created_utc: null
              due_utc: null
              group_ulid: null
              metadata: null
              modified_utc: ''
              notes:
              - body: Sample note 1 is quite long so we can observer how automatic wrapping can
                  produce unexpected results.
                ulid: 01hw5n9m99papg470w8j9k9vd3
              - body: "\nSample note 2 is short\nbut has  \nsurprising  \nline breaks."
                ulid: 01hw5n9y3q27zys83b139s7e2r
              priority: null
              ready_utc: null
              recurrence_duration: null
              repetition_duration: null
              review_utc: null
              state: null
              tags:
              - tag1
              - tag2
              ulid: 01hs68z7mdg4ktpxbv0yfafznq
              user: ''
              waiting_utc: null
            |]
            <> "\n"

      taskYaml `shouldBe` expected
