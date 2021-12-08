let log x = printfn "%A" x
let debugl x = System.IO.File.WriteAllLines("debug.txt", (x |> Seq.toArray))
let json s = System.Text.Json.JsonSerializer.Serialize s
let debugs s = System.IO.File.AppendAllText("debug.json", json s); s
let lines = System.IO.File.ReadAllLines "sample.txt"
let split (c: char) (s: string) = s.Split c
let replace (a: string) (b: string) (s: string) = s.Replace(a, b)
let isWhitespace (s: string) = System.String.IsNullOrWhiteSpace s

type Cell = { value: int; marked: bool; col: int; row: int; board: int }

let calls = lines |> Seq.head |> split ',' |> Seq.map int

let parseBingoRow s =
    s 
    |> split ' '
    |> Seq.filter (not << isWhitespace)

let boards = 
    lines
    |> Seq.filter (not << Seq.isEmpty)
    |> Seq.skip 1
    |> Seq.map parseBingoRow
    |> Seq.chunkBySize 5

let cells = [
    for (bi, board) in boards |> Seq.indexed do
        for (ri, row) in board |> Seq.indexed do
            for (ci, value) in row |> Seq.indexed do
                {value = int value; marked = false; col = ci; row = ri; board = bi}
]

cells
|> Seq.filter (fun cell -> cell.board = 0)
|> Seq.filter (fun cell -> cell.col = 0)
|> Seq.map (fun cell -> cell.value)