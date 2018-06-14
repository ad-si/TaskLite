module Utils where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal


data Filter a = NoFilter | Only a
  deriving (Eq, Ord, Show)


(<++>) :: Doc ann -> Doc ann -> Doc ann
x <++> y =
  x <> Char ' ' <> Char ' ' <> y
