module Constraint exposing (..)

{-|
@docs Constraint
@docs (<&>), (>>=)
@docs andThen, ap, ask, bind, constraints, map, pam, pure, run, with
-}

{-|
This type goes by many names: Reader, Kleisli, Function, etc.

This is simply a wrapper around `a -> b`.
-}
type Constraint r a
  = Constraint (r -> a)

{-|
Unwraps the `Constraint` constructor.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
run : Constraint r a -> r -> a
run (Constraint f) =
  f

{-|
Like `run` but flipped.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
with : r -> Constraint r a -> a
with =
  flip run

{-|
Threads the input directly through the output.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
ask : Constraint a a
ask =
  Constraint identity

{-|
An alias for `ask` that sometimes makes things easier to read.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
constraints : Constraint a a
constraints =
  Constraint identity

{-|
Transforms the output with the given function.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
map : (a -> b) -> Constraint r a -> Constraint r b
map f (Constraint g) =
  Constraint (f << g)

{-|
Like `map` but flipped.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
pam : Constraint r a -> (a -> b) -> Constraint r b
pam =
  flip map

{-|
An operator alias for `pam`.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
(<&>) : Constraint r a -> (a -> b) -> Constraint r b
(<&>) =
  flip map

{-|
Applies the output of a `Constraint` to the output of another `Constraint`
-}
ap : Constraint r (a -> b) -> Constraint r a -> Constraint r b
ap (Constraint f) (Constraint g) =
  Constraint (\r -> f r (g r))

{-|
Embeds a value in a `Constraint`.

Due to parametricity, this documentation is worthless,
as there is exactly one implementation of this function.
-}
pure : a -> Constraint r a
pure a =
  Constraint (\_ -> a)

{-|
Substitutes a value in a `Constraint`.
-}
bind : (a -> Constraint r b) -> Constraint r a -> Constraint r b
bind f (Constraint g) =
  Constraint (\r -> run (f (g r)) r)

{-|
Like `bind` but flipped.
-}
andThen : Constraint r a -> (a -> Constraint r b) -> Constraint r b
andThen =
  flip bind

{-|
An operator alias for `andThen`.
-}
(>>=) : Constraint r a -> (a -> Constraint r b) -> Constraint r b
(>>=) =
  flip bind
