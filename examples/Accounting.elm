module Examples.Accounting exposing (..)

import Constraint exposing (..)

hole : a
hole = Debug.crash "hole"

type HOLE = HOLE

type ClientOrderSheet = ClientOrderSheet
type Order = Order
type Market = Market
type Account = Account
type Execution = Execution
type Trade = Trade

clientOrders : ClientOrderSheet -> List Order
clientOrders =
  hole

execute : Market -> Account -> Order -> List Execution
execute =
  hole

allocate : List Account -> Execution -> List Trade
allocate =
  hole

tradeGeneration : (Market, Account, List Account) -> ClientOrderSheet -> List Trade
tradeGeneration (market, broker, clientAccounts) orderSheet =
  clientOrders orderSheet
    |> List.concatMap (execute market broker)
    |> List.concatMap (allocate clientAccounts)

type alias Environment env value = Constraint env (List value)

(>=>) : Environment a b -> Environment b c -> Environment a c
(>=>) (Constraint f) (Constraint g) =
  ask <&> (\a -> List.concatMap g (f a))

clientOrders_ : Environment ClientOrderSheet Order
clientOrders_ =
  hole

execute_ : (Market, Account) -> Environment Order Execution
execute_ =
  hole

allocate_ : List Account -> Environment Execution Trade
allocate_ =
  hole

tradeGeneration_ : (Market, Account, List Account) -> Environment ClientOrderSheet Trade
tradeGeneration_ (market, broker, clientAccounts) =
  clientOrders_
    >=> execute_ (market, broker)
    >=> allocate_ clientAccounts
