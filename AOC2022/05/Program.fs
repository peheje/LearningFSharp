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

let makeMove (source: char list) (destination: char list) =
    let toMove = source |> List.head
    let newDestination = toMove :: destination
    let newSource = source |> List.tail
    (newSource, newDestination)

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

for move in moves do
    let times = move[0]
    let sourceIdx = move[1]
    let destIdx = move[2]

    for n in 0..times do
        let (newSource, newDestination) = makeMove crates[sourceIdx] crates[destIdx]
        crates[sourceIdx] <- newSource
        crates[destIdx] <- newDestination
    
    printfn "move %i from %i to %i" times (move[1]+1) (move[2]+1)
    printfn "%A" crates

printfn "%A" crates

crates |> Array.map (fun col -> col |> List.head) |> System.String