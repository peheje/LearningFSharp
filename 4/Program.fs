let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s
let split (c: char) (s: string) = s.Split c
let isWhitespace (s: string) = System.String.IsNullOrWhiteSpace s
let parseRow s = s |> split ' ' |> Array.filter (not << isWhitespace)

type Cell = { value: int; mutable marked: bool; col: int; row: int; board: int }

let size = 5
let lines = System.IO.File.ReadAllLines "4.txt"
let calls = lines |> Array.head |> split ',' |> Array.map int

let boardsList = 
    lines
    |> Array.filter (not << Seq.isEmpty)
    |> Array.skip 1
    |> Array.map parseRow
    |> Array.chunkBySize size

let cells = [|
    for (bi, board) in boardsList |> Array.indexed do
        for (ri, row) in board |> Array.indexed do
            for (ci, value) in row |> Array.indexed do
                {value = int value; marked = false; col = ci; row = ri; board = bi}
|]

let boards = cells |> Array.groupBy (fun c -> c.board) |> Array.map (fun t -> snd t)

let mutable bingoBoards = Set.empty
for call in calls do
    for cell in cells do
        if cell.value = call then cell.marked <- true

    for (bi, board) in boards |> Array.indexed do
        let marked = board |> Array.filter (fun c -> c.marked)
        let bingoRow = marked |> Array.groupBy (fun c -> c.row) |> Array.exists (fun g -> ((snd g) |> Array.length) = size)
        let bingoCol = marked |> Array.groupBy (fun c -> c.col) |> Array.exists (fun g -> ((snd g) |> Array.length) = size)

        if bingoRow || bingoCol then
            if not (bingoBoards |> Set.contains bi) then
                bingoBoards <- bingoBoards |> Set.add bi
                let unmarkedOnBoard = board |> Array.filter (fun c -> not c.marked) |> Array.sumBy (fun c -> c.value)
                printfn $"Bingo on board {bi} = {unmarkedOnBoard * call}"