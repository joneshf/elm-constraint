module Constraint.Monoid exposing (..)

{-|
@docs Monoid
@docs nTimes
-}

import Constraint exposing (Constraint, (<&>), (>>=), ask)
import Constraint.Semigroup exposing (Semigroup)

{-|
Generalized identity.
-}
type alias Monoid a r =
  Semigroup a
    { r
    | identity : a
    }

{-|
Concatenate a value `n` times.
-}
nTimes : Int -> a -> Constraint (Monoid a r) a
nTimes n x =
  if n < 1 then
    ask <&> .identity
  else
    ask >>= \{concat, identity} ->
      nTimes (n - 1) x <&> concat x
