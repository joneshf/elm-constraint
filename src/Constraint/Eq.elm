module Constraint.Eq exposing (..)

{-|
@docs Eq
@docs eqEither, eqList, eqMaybe
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

{-|
If we have an `Eq a r`, then we can make an `Eq (Maybe a) r`.
-}
eqMaybe : Eq a r -> Eq (Maybe a) r
eqMaybe constraint =
  { constraint
  | eq = \x y ->
    case (x, y) of
      (Nothing, Nothing) ->
        True
      (Just x', Just y') ->
        constraint.eq x' y'
      _ ->
        False
  }

{-|
If we have an `Eq a r` and an `Eq b r`, then we can make an `Eq (Result a b) r`.
-}
eqEither : Eq a r -> Eq b r -> Eq (Result a b) r
eqEither aConstraint bConstraint =
  { aConstraint
  | eq = \x y ->
    case (x, y) of
      (Err x', Err y') ->
        aConstraint.eq x' y'
      (Ok x', Ok y') ->
        bConstraint.eq x' y'
      _ ->
        False
  }

{-|
If we have an `Eq a r`, then we can make an `Eq (List a) r`.
-}
eqList : Eq a r -> Eq (List a) r
eqList constraint =
  let equateLists list list' =
    case (list, list') of
      ([], []) ->
        True
      (x::xs, y::ys) ->
        constraint.eq x y && equateLists xs ys
      _ ->
        False
  in
    { constraint
    | eq = equateLists
    }
