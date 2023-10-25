module Equation

open Browser.Types
open Html

type private Expression =
    | Number of float
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Abs of Expression

let rec private evaluate expression =
    match expression with
    | Number n -> n
    | Add(x, y) -> evaluate x + evaluate y
    | Subtract(x, y) -> evaluate x - evaluate y
    | Multiply(x, y) -> evaluate x * evaluate y
    | Abs x -> System.Math.Abs(evaluate x)

let private initRandom () =
    let random = System.Random()
    fun min max -> random.Next(min, max)

let private random = initRandom ()

let rec private generateTree depth =
    if depth = 0 then
        Number(random -10 11)
    else
        let left = generateTree (depth - 1)
        let right = generateTree (depth - 1)

        match (random 0 4), (random 0 10) with
        | 0, _ -> Add(left, right)
        | 1, _ -> Subtract(left, right)
        | 2, _ -> Multiply(left, right)
        | 3, 0 -> Abs(left)
        | _ -> generateTree depth

let private getExpression tree =
    let rec printEquation' tree =
        match tree with
        | Number n -> string n
        | Add(left, right) -> printBinary left right "+"
        | Subtract(left, right) -> printBinary left right "-"
        | Multiply(left, right) -> printBinary left right "*"
        | Abs(left) -> printUnary left "abs"

    and printBinary left right symbol =
        "(" + printEquation' left + symbol + printEquation' right + ")"

    and printUnary left symbol =
        symbol + "(" + printEquation' left + ")"

    printEquation' tree + "=" + (tree |> evaluate |> string)

let private replaceNumberWithX input =
    let matches = System.Text.RegularExpressions.Regex.Matches(input, "\d+")

    if matches.Count > 0 then
        let index = random 0 matches.Count
        let m = matches[index]

        let equation =
            input.Substring(0, m.Index) + "X" + input.Substring(m.Index + m.Length)

        (equation, m.Value)
    else
        (input, "")

let mutable private answer = ""

let private generateEquation () =
    let depthInput = (fromId "depth") :?> HTMLInputElement
    let depth = int depthInput.value
    let tree = generateTree depth
    let expression = getExpression tree
    let equation, replacedValue = replaceNumberWithX expression
    answer <- replacedValue
    let equationSpan = fromId "equation"
    equationSpan.innerText <- equation

let private checkAnswer () =
    let guessInput = fromId "guess" :?> HTMLInputElement
    let guess = guessInput.value
    let resultDiv = fromId "result"

    if guess = answer then
        resultDiv.innerText <- "Correct! The answer is indeed " + string answer + "."
    else
        resultDiv.innerText <- "Incorrect. The correct answer was " + string answer + "."

let private checkMaxDepth (element: HTMLElement) =
    let input = element :?> HTMLInputElement
    input.valueAsNumber <- System.Math.Clamp(input.valueAsNumber, 1, 10)

let initEquation () =
    fromId "depth" |> onChangeElement checkMaxDepth
    fromId "generate-btn" |> onClick generateEquation
    fromId "check-btn" |> onClick checkAnswer
