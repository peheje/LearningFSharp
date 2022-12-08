let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/05/input"
let raw = (System.IO.File.ReadAllText path).Split("\n\n")
let cratesRaw, movesRaw = raw[0], raw[1]

let parseCrates (row: string) =
    row
        .Replace("    ", "#")
        .Replace("[", "")
        .Replace("]", "")
        .Replace(" ", "")
    |> Seq.toArray

let crates =
    cratesRaw.Split("\n")
    |> Seq.filter (fun row -> row.Contains("["))
    |> Seq.map parseCrates
    |> Seq.transpose
    |> Seq.map (fun r -> r |> Seq.filter (fun x -> x <> '#') |> Seq.toList)

let moves =
    movesRaw
        .Replace("move ", "")
        .Replace(" from", "")
        .Replace(" to", "")
        .Split("\n")
    |> Array.map (fun row ->
        row.Split(" ")
        |> Array.map (fun d -> (d |> int) - 1))

// Part 1
let makeMove source destination =
    match source with
    | x :: xs -> (xs, x :: destination)
    | _ -> failwith "cannot move"

let crates1 = crates |> Seq.toArray

for move in moves do
    let times = move[0]
    let sourceIdx = move[1]
    let destIdx = move[2]

    for n in 0..times do
        let (nextSource, nextDestination) = makeMove crates1[sourceIdx] crates1[destIdx]
        crates1[sourceIdx] <- nextSource
        crates1[destIdx] <- nextDestination

let part1 =
    crates1
    |> Array.map (fun col -> col |> List.head)
    |> System.String

// Part 2
let makeMoves source destination times =
    let toMove = source |> List.take times
    let newSource = source |> List.removeManyAt 0 times
    let newDestination = toMove @ destination
    (newSource, newDestination)

let crates2 = crates |> Seq.toArray

for move in moves do
    let times = move[0] + 1
    let sourceIdx = move[1]
    let destIdx = move[2]

    let (nextSource, nextDestination) =
        makeMoves crates2[sourceIdx] crates2[destIdx] times

    crates2[sourceIdx] <- nextSource
    crates2[destIdx] <- nextDestination

let part2 =
    crates2
    |> Array.map (fun col -> col |> List.head)
    |> System.String
