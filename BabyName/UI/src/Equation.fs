module Equation

type Expression =
| Number of int
| Add of Expression * Expression
| Subtract of Expression * Expression
| Multiply of Expression * Expression

let rec evaluate expression =
    match expression with
    | Number n -> n
    | Add (x, y) -> evaluate x + evaluate y
    | Subtract (x, y) -> evaluate x - evaluate y
    | Multiply (x, y) -> evaluate x * evaluate y

let random = System.Random()

let rec generateTree depth =
    if depth = 0 then
        Number (random.Next(1, 10))
    else
        let left = generateTree (depth - 1)
        let right = generateTree (depth - 1)
        match random.Next(0, 3) with
        | 0 -> Add (left, right)
        | 1 -> Subtract (left, right)
        | 2 -> Multiply (left, right)

let printTree tree =
    let rec printTree' tree =
        match tree with
        | Number n -> printf "%d" n
        | Add (left, right) -> printBinary left right "+"
        | Subtract (left, right) -> printBinary left right "-"
        | Multiply (left, right) -> printBinary left right "*"
    and
        printBinary left right symbol =
            printf "("; printTree' left; printf "%s" symbol; printTree' right; printf ")"

    printTree' tree
    printfn ""

let t0 = generateTree 3
printTree t0
evaluate t0