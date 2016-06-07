module Constraint.Ord exposing (..)

{-|
@docs Ord
-}

{-|
Generalized ordering.
-}
type alias Ord a r =
  { r
  | compare : a -> a -> Order
  }
