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

let hasBingo board  =
    let bingo selector xs = xs |> Array.map selector |> Array.countBy id |> Array.exists (fun x -> snd x = size)
    let marked = board |> Array.filter (fun c -> c.marked)
    marked |> bingo (fun c -> c.row) || marked |> bingo (fun c -> c.col)

let boards = cells |> Array.groupBy (fun c -> c.board) |> Array.map (fun t -> snd t)

let mutable bingoBoards = Set.empty
for call in calls do
    
    for cell in cells do if cell.value = call then cell.marked <- true

    for (bi, board) in boards |> Array.indexed do
        if board |> hasBingo then
            if not (bingoBoards |> Set.contains bi) then
                bingoBoards <- bingoBoards |> Set.add bi
                let sumOfUnmarked = board |> Array.filter (fun c -> not c.marked) |> Array.sumBy (fun c -> c.value)
                printfn $"Bingo on board {bi} = {sumOfUnmarked * call}"