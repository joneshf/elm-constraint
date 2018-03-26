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
      (Just x_, Just y_) ->
        constraint.eq x_ y_
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
      (Err x_, Err y_) ->
        aConstraint.eq x_ y_
      (Ok x_, Ok y_) ->
        bConstraint.eq x_ y_
      _ ->
        False
  }

{-|
If we have an `Eq a r`, then we can make an `Eq (List a) r`.
-}
eqList : Eq a r -> Eq (List a) r
eqList constraint =
  let equateLists list list_ =
    case (list, list_) of
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
