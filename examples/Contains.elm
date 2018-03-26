module Examples.Contains exposing (..)

import Constraint exposing (..)
import Constraint.Eq exposing (Eq)
import Constraint.Monoid exposing (Monoid)

contains : a -> List a -> Bool
contains y xs =
  case xs of
    x::xss ->
      x == y || contains y xss
    [] ->
      False

contains_ : a -> List a -> Constraint (Eq a r) Bool
contains_ y xs =
  ask >>= \c ->
    case xs of
      x::xss ->
        contains_ y xss <&> \b ->
          c.eq x y || b
      [] ->
        pure False

sum : List number -> number
sum ns =
  case ns of
    n::nss ->
      n + sum nss
    [] ->
      0

sum_ : List a -> Constraint (Monoid a r) a
sum_ ns =
  ask >>= \c ->
    case ns of
      n::nss ->
        sum_ nss <&> c.concat n
      [] ->
        pure c.identity

sumIfContained : a -> List a -> Constraint (Monoid a (Eq a r)) a
sumIfContained x xs =
  contains_ x xs >>= \b ->
    if b then
      sum_ xs
    else
      sum_ []

total : Constraint (Monoid number {}) number
total =
  sum_ [1,2,3]

six : Int
six =
  run total {identity = 0, concat = (+)}

notContained : Constraint (Monoid String (Eq String r)) String
notContained =
  sumIfContained "wat" ["This", "is", "it"]

contained : Constraint (Monoid String (Eq String r)) String
contained =
  sumIfContained "is" ["This", "is", "it"]

empty : String
empty =
  run notContained {concat = (++), eq = (==), identity = ""}

notEmpty : String
notEmpty =
  run contained {concat = (++), eq = (==), identity = ""}
