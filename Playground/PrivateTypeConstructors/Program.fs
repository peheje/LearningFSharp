module Types =

    type Amount =
        private
        | Amount of int
        static member Create x =
            if x < 0 then
                raise (exn "Amount cannot be negative")
            else
                Amount x

    type Limbs =
        private
            { legs: int
              disabled: bool }
        static member Create legs disabled =
            if legs <> 2 && not disabled then
                raise (exn "Must be disabled if you don't have two legs")
            else
                { legs = legs; disabled = disabled }

open Types

Amount.Create 1

Amount.Create -1

Limbs.Create 2 false

Limbs.Create 1 false
