module Utils where

import Protolude as P

import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Internal


data Filter a = NoFilter | Only a
  deriving (Eq, Ord, Show)


(<++>) :: Doc ann -> Doc ann -> Doc ann
x <++> y =
  x <> softline <> softline <> y


(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) =
  flip (<$>)
