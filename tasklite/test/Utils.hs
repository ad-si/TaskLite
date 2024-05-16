{-# OPTIONS_GHC -Wno-deprecations #-}

module Utils where

import Protolude (pure, (.))
import Protolude.Error (error)

import Language.Haskell.TH (Exp (LitE), Lit (StringL))
import Language.Haskell.TH.Quote (
  QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
 )


raw :: QuasiQuoter
raw =
  QuasiQuoter
    { quoteExp = pure . LitE . StringL
    , quotePat = \_ ->
        error
          "Illegal raw string QuasiQuote \
          \(allowed as expression only, used as a pattern)"
    , quoteType = \_ ->
        error
          "Illegal raw string QuasiQuote \
          \(allowed as expression only, used as a type)"
    , quoteDec = \_ ->
        error
          "Illegal raw string QuasiQuote \
          \(allowed as expression only, used as a declaration)"
    }
