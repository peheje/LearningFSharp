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

let initRandom () =
    let random = System.Random()
    fun min max -> random.Next(min, max)

let random = initRandom ()

let rec generateTree depth =
    if depth = 0 then
        Number (random -10 11)
    else
        let left = generateTree (depth - 1)
        let right = generateTree (depth - 1)
        match random 0 4 with
        | 0 -> Add (left, right)
        | 1 -> Subtract (left, right)
        | 2 -> Multiply (left, right)
        | 3 -> Abs (left)

let printEquation tree =
    let rec printEquation' tree =
        match tree with
        | Number n -> string n
        | Add (left, right) -> printBinary left right "+"
        | Subtract (left, right) -> printBinary left right "-"
        | Multiply (left, right) -> printBinary left right "*"
        | Abs (left) -> printUnary left "abs"
    and
        printBinary left right symbol =
            "(" + printEquation' left + symbol + printEquation' right + ")"
    and
        printUnary left symbol =
            symbol + "(" + printEquation' left + ")"
    printEquation' tree

let replaceRandomMatch input =
    let matches = System.Text.RegularExpressions.Regex.Matches(input, "\d")
    if matches.Count > 0 then
        let index = random 0 matches.Count
        let m = matches[index]
        input.Substring(0, m.Index) + "X" + input.Substring(m.Index + m.Length)
    else
        input

let tree = generateTree 5
let expression = printEquation tree
let answer = evaluate tree
let equation = replaceRandomMatch expression

printfn "%A" tree 
printfn "%s" expression
printfn "%i" answer
printfn "%s" equation