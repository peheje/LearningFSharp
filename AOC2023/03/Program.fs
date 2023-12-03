let rows = System.IO.File.ReadAllLines "C:\Users\peter\Repos\LearningFSharp\AOC2023\03\data"

let log x = printfn "%A" x

let map =
    rows
    |> Array.map (fun row -> row |> Seq.toArray |> Array.map id)

let isSymbol symbol =
    symbol |> System.Char.IsDigit |> not
    && symbol <> '.'

let maxX = map[0].Length - 1
let maxY = map.Length - 1

let hasNeighborSymbol x y =
    (x > 0 && map[y][x - 1] |> isSymbol)
    || (x < maxX && map[y][x + 1] |> isSymbol)
    || (y > 0 && map[y - 1][x] |> isSymbol)
    || (y < maxY && map[y + 1][x] |> isSymbol)
    || (x > 0 && y > 0 && map[y - 1][x - 1] |> isSymbol)
    || (y < maxY && x < maxX && map[y + 1][x + 1] |> isSymbol)
    || (y < maxY && x > 0 && map[y + 1][x - 1] |> isSymbol)
    || (y > 0 && x < maxX && map[y - 1][x + 1] |> isSymbol)

let part1 =
    seq {

        for (y, row) in map |> Array.indexed do

            let sb = System.Text.StringBuilder()
            let mutable valid = false

            for (x, c) in row |> Array.indexed do
                if c |> System.Char.IsDigit then
                    sb.Append c |> ignore

                    if hasNeighborSymbol x y then
                        valid <- true
                else
                    if sb.Length <> 0 && valid then
                        yield (sb.ToString() |> int)

                    valid <- false
                    sb.Clear() |> ignore
            
            if sb.Length <> 0 && valid then
                yield (sb.ToString() |> int)

    }
    |> Seq.sum
