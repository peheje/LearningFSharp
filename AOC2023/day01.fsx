let input = System.IO.File.ReadAllText "data01"

let rows = input.Split System.Environment.NewLine

let firstAndLastDigit row =
    let first = row |> Seq.find System.Char.IsDigit
    let last = row |> Seq.findBack System.Char.IsDigit
    let concatenated = sprintf "%c%c" first last
    concatenated |> int

rows |> Seq.map firstAndLastDigit |> Seq.sum