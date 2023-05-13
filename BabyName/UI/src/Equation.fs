module Equation
open Browser.Types
open Html
open Browser

type Expression =
| Number of float
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

let private generateEquation () =
    let depthInput = (fromId "depth") :?> HTMLInputElement
    let depth = int depthInput.value
    let tree = generateTree depth
    let answer = evaluate tree
    let expression = printEquation tree
    let equation = replaceRandomMatch expression
    window.localStorage.setItem("answer", string answer)
    let equationSpan = fromId "equation"
    equationSpan.innerText <- equation

let private checkAnswer () =
    let guessInput = fromId "guess" :?> HTMLInputElement
    let guess = guessInput.value
    let answer = localStorage.getItem("answer")
    let resultDiv = fromId "result"
    if guess = answer then
        resultDiv.innerText <- "Correct! The answer is indeed " + string answer + "."
    else
        resultDiv.innerText <- "Incorrect. The correct answer was " + string answer + "."

let initEquation () =
    fromId "generate-btn" |> onClick generateEquation
    fromId "check-btn" |> onClick checkAnswer