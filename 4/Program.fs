let log x = printfn "%A" x
let logs x = printfn "%A" x; x
let debugl x = System.IO.File.WriteAllLines("debug.txt", (x |> Seq.toArray))
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.AppendAllText("debug.json", json s); s
let lines = System.IO.File.ReadAllLines "sample.txt"
let split (c: char) (s: string) = s.Split c
let replace (a: string) (b: string) (s: string) = s.Replace(a, b)
let isWhitespace (s: string) = System.String.IsNullOrWhiteSpace s
let countWhere a = Seq.filter a >> Seq.length

type Cell = { value: int; mutable marked: bool; col: int; row: int; board: int }

let calls = lines |> Seq.head |> split ',' |> Seq.map int

let parseBingoRow s =
    s 
    |> split ' '
    |> Seq.filter (not << isWhitespace)

let boardsList = 
    lines
    |> Seq.filter (not << Seq.isEmpty)
    |> Seq.skip 1
    |> Seq.map parseBingoRow
    |> Seq.chunkBySize 5

let cells = [
    for (bi, board) in boardsList |> Seq.indexed do
        for (ri, row) in board |> Seq.indexed do
            for (ci, value) in row |> Seq.indexed do
                {value = int value; marked = false; col = ci; row = ri; board = bi}
]

let nRows = (cells |> Seq.map (fun c -> c.row) |> Seq.max) + 1
let nCols = (cells |> Seq.map (fun c -> c.col) |> Seq.max) + 1

let boards = cells |> Seq.groupBy (fun c -> c.board) |> Seq.map (fun t -> snd t)

for call in calls do
    for cell in cells do
        if cell.value = call then cell.marked <- true

    for board in boards do
        let marked = board |> Seq.filter (fun c -> c.marked)
        let bingoRow = marked |> Seq.groupBy (fun c -> c.row) |> Seq.exists (fun g -> ((snd g) |> Seq.length) = nCols)
        let bingoCol = marked |> Seq.groupBy (fun c -> c.col) |> Seq.exists (fun g -> ((snd g) |> Seq.length) = nRows)

        if bingoRow || bingoCol then
            let unmarkedOnBoard = board |> Seq.filter (fun c -> not c.marked) |> Seq.sumBy (fun c -> c.value)
            printfn $"bingo ${unmarkedOnBoard * call}"
            failwith "end"


