module Constraint.Eq exposing (..)

{-|
@docs Eq
@docs (===), (=/=)
-}

import Constraint exposing (Constraint, (<&>), ask)

{-|
Generalized equality.
-}
type alias Eq a r =
  { r
  | eq : a -> a -> Bool
  }

{-|
Operator alias for `eq`.
-}
(===) : a -> a -> Constraint (Eq a r) Bool
(===) x y =
  ask <&> \{eq} ->
    eq x y

{-|
Negation of `(===)`.
-}
(=/=) : a -> a -> Constraint (Eq a r) Bool
(=/=) x y =
  ask <&> \{eq} ->
    not (eq x y)
