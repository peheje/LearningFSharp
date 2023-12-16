let rowToMatches (row: string) =
    let numbers = row.Split(": ")[1]

    let parseNumbers (numbers: string) =
        numbers.Split(' ')
        |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
        |> Seq.map int
        |> Set.ofSeq

    numbers.Split('|')
    |> Array.map parseNumbers
    |> Set.intersectMany
    |> Set.count

let rowToPoint (row: string) =
    2.0 ** ((row |> rowToMatches |> float) - 1.0) |> int

let rows = System.IO.File.ReadAllLines "data"

let part1 = rows |> Array.sumBy rowToPoint
printfn "Part 1 %i" part1

let rowToCard (row: string) =
    let cardNumber = (row.Split(": ")[0]).Replace("Card ", "") |> int
    (cardNumber, rowToMatches row)

let cardsSeq = rows |> Seq.map rowToCard
let cardsMap = cardsSeq |> Map.ofSeq

let rec followCards cardNumber =
    match cardsMap[cardNumber] with
    | 0 -> 0
    | won -> won + ([ cardNumber + 1 .. cardNumber + won ] |> Seq.sumBy followCards)

let part2 =
    (cardsSeq |> Seq.map (fst >> followCards) |> Seq.sum) + (cardsSeq |> Seq.length)

printfn "Part 2 %i" part2
