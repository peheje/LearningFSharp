let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s
let split (c: char) (s: string) = s.Split c
let isWhitespace (s: string) = System.String.IsNullOrWhiteSpace s
let parseRow s = s |> split ' ' |> Seq.filter (not << isWhitespace)

type Cell = { value: int; mutable marked: bool; col: int; row: int; board: int }

let size = 5
let lines = System.IO.File.ReadAllLines "4.txt"
let calls = lines |> Seq.head |> split ',' |> Seq.map int

let boardsList = 
    lines
    |> Seq.filter (not << Seq.isEmpty)
    |> Seq.skip 1
    |> Seq.map parseRow
    |> Seq.chunkBySize size

let cells = [
    for (bi, board) in boardsList |> Seq.indexed do
        for (ri, row) in board |> Seq.indexed do
            for (ci, value) in row |> Seq.indexed do
                {value = int value; marked = false; col = ci; row = ri; board = bi}
]

let boards = cells |> Seq.groupBy (fun c -> c.board) |> Seq.map (fun t -> snd t)

let mutable bingoBoards = Set.empty
for call in calls do
    for cell in cells do
        if cell.value = call then cell.marked <- true

    for (bi, board) in boards |> Seq.indexed do
        let marked = board |> Seq.filter (fun c -> c.marked)
        let bingoRow = marked |> Seq.groupBy (fun c -> c.row) |> Seq.exists (fun g -> ((snd g) |> Seq.length) = size)
        let bingoCol = marked |> Seq.groupBy (fun c -> c.col) |> Seq.exists (fun g -> ((snd g) |> Seq.length) = size)

        if bingoRow || bingoCol then
            if not (bingoBoards |> Set.contains bi) then
                bingoBoards <- bingoBoards |> Set.add bi
                let unmarkedOnBoard = board |> Seq.filter (fun c -> not c.marked) |> Seq.sumBy (fun c -> c.value)
                printfn $"Bingo on board {bi} = {unmarkedOnBoard * call}"