module Constraint.Ord exposing (..)

{-|
@docs Ord
@docs ordEither, ordList, ordMaybe
@docs max, min
-}

import Constraint exposing (..)

{-|
Generalized ordering.
-}
type alias Ord a r =
  { r
  | compare : a -> a -> Order
  }

{-|
Find the larger of two values.
-}
max : a -> a -> Constraint (Ord a r) a
max x y =
  ask <&> \c ->
    case c.compare x y of
      GT ->
        x
      _ ->
        y

{-|
Find the smaller of two values.
-}
min : a -> a -> Constraint (Ord a r) a
min x y =
  ask <&> \c ->
    case c.compare x y of
      LT ->
        x
      _ ->
        y

{-|
If we have an `Ord a r`, then we can make an `Ord (Maybe a) r`.
-}
ordMaybe : Ord a r -> Ord (Maybe a) r
ordMaybe constraint =
  { constraint
  | compare = \x y ->
    case (x, y) of
      (Nothing, Nothing) ->
        EQ
      (Nothing, Just _) ->
        LT
      (Just _, Nothing) ->
        GT
      (Just x_, Just y_) ->
        constraint.compare x_ y_
  }

{-|
If we have an `Ord a r` and an `Ord b r`, then we can make an `Ord (Result a b) r`.
-}
ordEither : Ord a r -> Ord b r -> Ord (Result a b) r
ordEither aConstraint bConstraint =
  { aConstraint
  | compare = \x y ->
    case (x, y) of
      (Err x_, Err y_) ->
        aConstraint.compare x_ y_
      (Err _, Ok _) ->
        LT
      (Ok _, Err _) ->
        GT
      (Ok x_, Ok y_) ->
        bConstraint.compare x_ y_
  }

{-|
If we have an `Ord a r`, then we can make an `Ord (List a) r`.
-}
ordList : Ord a r -> Ord (List a) r
ordList constraint =
  let compareLists list list_ =
    case (list, list_) of
      ([], []) ->
        EQ
      ([], _) ->
        LT
      (_, []) ->
        GT
      (x::xs, y::ys) ->
        case constraint.compare x y of
          EQ ->
            compareLists xs ys
          ordering ->
            ordering
  in
    { constraint
    | compare = compareLists
    }
