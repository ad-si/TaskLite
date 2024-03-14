module Main where

import Protolude (
  Bool (False, True),
  IO,
  Maybe (Just, Nothing),
  Monad (return),
  Ord ((>)),
  head,
  identity,
  map,
  ($),
  (<$>),
 )

import Test.HUnit (Assertion, Test (..), runTestTT, (@?=))
import Text.Huzzy as Hu (
  CaseSensitivity (HandleCase, IgnoreCase),
  Fuzzy (original, rendered, score),
  filter,
  match,
  test,
 )


from :: [Assertion] -> Test
from xs = TestList (map TestCase xs)


tests :: Test
tests =
  TestList
    [ TestLabel "test" $
        TestList
          [ TestLabel "should return true when fuzzy match" $
              from
                [ Hu.test "back" "imaback" @?= True
                , Hu.test "back" "bakck" @?= True
                , Hu.test "shig" "osh kosh modkhigow" @?= True
                , Hu.test "" "osh kosh modkhigow" @?= True
                ]
          , TestLabel "should return false when no fuzzy match" $
              from
                [ Hu.test "back" "abck" @?= False
                , Hu.test "okmgk" "osh kosh modkhigow" @?= False
                ]
          ]
    , TestLabel "match" $
        TestList
          [ TestLabel
              "should return a greater score for consecutive matches of pattern"
              $ from
                [ (>)
                    (Hu.score <$> Hu.match IgnoreCase ("", "") identity "abcd" "zabcd")
                    (Hu.score <$> Hu.match IgnoreCase ("", "") identity "abcd" "azbcd")
                    @?= True
                ]
          , TestLabel
              "should return the string as is if no pre/post and case sensitive"
              $ from
                [ Hu.rendered
                    <$> Hu.match
                      HandleCase
                      ("", "")
                      identity
                      "ab"
                      "ZaZbZ"
                      @?= Just "ZaZbZ"
                ]
          , TestLabel "should return Nothing on no match" $
              from
                [ Hu.match
                    IgnoreCase
                    ("", "")
                    identity
                    "ZEBRA!"
                    "ZaZbZ"
                    @?= Nothing
                ]
          , TestLabel "should be case sensitive is specified" $
              from
                [ Hu.match
                    HandleCase
                    ("", "")
                    identity
                    "hask"
                    "Haskell"
                    @?= Nothing
                ]
          , TestLabel "should be wrap pre and post around matches" $
              from
                [ Hu.rendered
                    <$> Hu.match
                      HandleCase
                      ("<", ">")
                      identity
                      "brd"
                      "bread"
                      @?= Just "<b><r>ea<d>"
                ]
          ]
    , TestLabel "filter" $
        TestList
          [ TestLabel "should return list untouched when given empty pattern" $
              from
                [ map
                    Hu.original
                    (Hu.filter HandleCase ("", "") identity "" ["abc", "def"])
                    @?= ["abc", "def"]
                ]
          , TestLabel "should return the highest score first" $
              from
                [ (@?=)
                    (head (Hu.filter HandleCase ("", "") identity "cb" ["cab", "acb"]))
                    (head (Hu.filter HandleCase ("", "") identity "cb" ["acb"]))
                ]
          ]
    ]


runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()


-- | For now, main will run our tests.
main :: IO ()
main = runTests
