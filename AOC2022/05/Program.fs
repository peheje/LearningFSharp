let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/05/input"
let raw = System.IO.File.ReadAllText path

let s = raw.Split "\n\n"

let parseCrates (row: string) =
    row
        .Replace("    ", "#")
        .Replace("[", "")
        .Replace("]", "")
        .Replace(" ", "")
    |> Seq.toArray

let makeMove source destination =
    match source with
    | toMove :: tailOfSource ->
        (tailOfSource, toMove :: destination)
    | _ -> failwith "cannot move"

let crates =
    s[ 0 ].Split("\n")
    |> Array.filter (fun row -> row.Contains("["))
    |> Array.map parseCrates
    |> Array.transpose
    |> Array.map (fun r -> r |> Seq.filter (fun x -> x <> '#') |> Seq.toList)

let moves =
    s[1]
        .Replace("move ", "")
        .Replace(" from", "")
        .Replace(" to", "")
        .Split("\n")
    |> Array.map (fun row ->
        row.Split(" ")
        |> Array.map (fun d -> (d |> int) - 1))

// Mutates crates
for move in moves do
    let times = move[0]
    let sourceIdx = move[1]
    let destIdx = move[2]

    for n in 0..times do
        let (nextSource, nextDestination) = makeMove crates[sourceIdx] crates[destIdx]
        crates[sourceIdx] <- nextSource
        crates[destIdx] <- nextDestination
    
crates |> Array.map (fun col -> col |> List.head) |> System.String