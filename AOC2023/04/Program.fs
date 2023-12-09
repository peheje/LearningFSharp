let rowToMatches (row: string) =
    let cardNumberIndex = row.IndexOf(": ")
    let numbers = row.Substring(cardNumberIndex + 2)
    let split = numbers.Split('|')

    let parseNumbers (numbers: string) =
        numbers.Trim().Split(' ')
        |> Seq.map _.Trim()
        |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
        |> Seq.map int

    let winning = parseNumbers split[0] |> Set.ofSeq
    let ours = parseNumbers split[1] |> Set.ofSeq

    winning |> Set.intersect ours |> Set.count

let rowToPoint (row: string) =
    2.0 ** ((row |> rowToMatches |> float) - 1.0) |> int

let rows = System.IO.File.ReadAllLines "data"

let part1 = rows |> Array.sumBy rowToPoint
printfn "Part 1 %i" part1

let rowToCard (row: string) =
    let cardNumberIndex = row.IndexOf(": ")
    let cardNumber = row.Substring(0, cardNumberIndex).Replace("Card ", "") |> int
    (cardNumber, rowToMatches row)

let cardsSeq = rows |> Seq.map rowToCard
let cardsMap = cardsSeq |> Map.ofSeq

let rec followCards cardNumber =
    match cardsMap[cardNumber] with
    | 0 -> 0
    | won ->
        won
        + ([ cardNumber + 1 .. cardNumber + won ]
           |> Seq.sumBy followCards)

let part2 =
    (cardsSeq |> Seq.map (fst >> followCards) |> Seq.sum) + (cardsSeq |> Seq.length)

printfn "Part 2 %i" part2
