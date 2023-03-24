module Equation

type Expression =
| Number of int
| Add of Expression * Expression
| Subtract of Expression * Expression
| Multiply of Expression * Expression
| Abs of Expression

let rec evaluate expression =
    match expression with
    | Number n -> n
    | Add (x, y) -> evaluate x + evaluate y
    | Subtract (x, y) -> evaluate x - evaluate y
    | Multiply (x, y) -> evaluate x * evaluate y
    | Abs x -> System.Math.Abs (evaluate x)

let randomNumber min max =
    let random = System.Random()
    fun () -> random.Next(min, max + 1)

let rec generateTree depth =
    if depth = 0 then
        Number (randomNumber -10 10 ())
    else
        let left = generateTree (depth - 1)
        let right = generateTree (depth - 1)
        match randomNumber 0 3 () with
        | 0 -> Add (left, right)
        | 1 -> Subtract (left, right)
        | 2 -> Multiply (left, right)
        | 3 -> Abs (left)

let printEquation tree =
    let rec printEquation' tree =
        match tree with
        | Number n -> printf "%d" n
        | Add (left, right) -> printBinary left right "+"
        | Subtract (left, right) -> printBinary left right "-"
        | Multiply (left, right) -> printBinary left right "*"
        | Abs (left) -> printUnary left "abs"
    and
        printBinary left right symbol =
            printf "("; printEquation' left; printf "%s" symbol; printEquation' right; printf ")"
    and
        printUnary left symbol =
            printf "%s(" symbol; printEquation' left; printf ")"


    printEquation' tree
    printfn ""

let t0 = generateTree 5
printEquation t0
evaluate t0