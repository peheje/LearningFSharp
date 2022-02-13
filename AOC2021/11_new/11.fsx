type Octo = {power: int; flashed: bool}

let data =
    System.IO.File.ReadAllLines("sample.txt")
    |> Array.map (fun row -> [|for x in row -> int (string x)|])
    |> Array.map (fun row -> row |> Array.map (fun x -> {power = x; flashed = false}))

let surrounding row col =
    let lastIndex = data |> Array.length
    let minBoundary x = max 0 (x - 1)
    let maxBoundary x = min lastIndex (x + 1)
    let maxMin x = [|minBoundary x..maxBoundary x|]
    Array.allPairs (maxMin row) (maxMin col)
        |> Array.filter ((<>) (row, col))

let mapper state row =
    printfn "state is %A" state
    printfn "row is %A" row
    (row, 1)

data |> Array.mapFold mapper 0