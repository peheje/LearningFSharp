
module String =
    let trim (x: string) = x.Trim()
    let split (c: char) (x: string) = x.Split c
    let indexOf (s: string) (x: string) = x.IndexOf s
    let substring (i: int) (x: string) = x.Substring i

let rowToPoint row =
    let cardNumberIndex = row |> String.indexOf ": "
    let numbers = row |> String.substring (cardNumberIndex + 2)
    let split = numbers |> String.split '|'

    let notEmpty (x: string) =
        System.String.IsNullOrWhiteSpace x |> not

    let parseNumbers numbers =
        numbers
        |> String.trim
        |> String.split ' '
        |> Array.map String.trim
        |> Array.filter notEmpty
        |> Array.map int

    let winning = parseNumbers split[0] |> Set.ofArray
    let ours = parseNumbers split[1] |> Set.ofArray

    let intersection = winning |> Set.intersect ours
    2.0**((intersection |> Set.count |> float) - 1.0) |> int

let rows = System.IO.File.ReadAllLines @"C:\Users\peter\repos\LearningFSharp\AOC2023\04\data"

rows |> Array.sumBy rowToPoint