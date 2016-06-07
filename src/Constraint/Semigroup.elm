module Constraint.Semigroup exposing (..)

{-|
@docs Semigroup
@docs (++)
-}

import Constraint exposing (Constraint, (<&>), ask)

{-|
Generalized concatenation.
-}
type alias Semigroup a r =
  { r
  | concat : a -> a -> a
  }

{-|
Operator alias for `concat`.
-}
(++) : a -> a -> Constraint (Semigroup a r) a
(++) x y =
  ask <&> \{concat} ->
    concat x y
