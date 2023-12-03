let input = System.IO.File.ReadAllText "data01"

let rows = input.Split System.Environment.NewLine

// Part 1
(*
let firstAndLastDigit row =
    let first = row |> Seq.find System.Char.IsDigit
    let last = row |> Seq.findBack System.Char.IsDigit
    let concatenated = sprintf "%c%c" first last
    concatenated |> int

rows |> Seq.map firstAndLastDigit |> Seq.sum
*)

// Part 2
let letters =
    Map [ "one", 1
          "two", 2
          "three", 3
          "four", 4
          "five", 5
          "six", 6
          "seven", 7
          "eight", 8
          "nine", 9
          "1", 1
          "2", 2
          "3", 3
          "4", 4
          "5", 5
          "6", 6
          "7", 7
          "8", 8
          "9", 9
          "0", 0 ]


let reverseString input =
    new string(input |> Seq.toArray |> Array.rev)

letters |> Map.map (fun k v -> (v, k))

let lettersReversed =
    letters
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> (k |> reverseString, v))
    |> Map

let rec firstDigit (letters: Map<string, int>) (row: string) =
    match letters |> Map.keys |> Seq.tryFind (fun key -> row.StartsWith key) with
    | None -> firstDigit letters (row.Substring(1))
    | Some v -> letters |> Map.find v |> string

let part2 = rows |> Seq.sumBy (fun row ->
    let first = firstDigit letters row
    let last = firstDigit lettersReversed (row |> reverseString)
    (first + last) |> int
)
