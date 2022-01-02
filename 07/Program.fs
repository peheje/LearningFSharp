let split c s = (s: string).Split(c: char)
let crabs = System.IO.File.ReadAllText("7.txt") |> split ',' |> Seq.map int
let positions = [0..crabs |> Seq.max]

// Part 1
let cost1 position =
    crabs |> Seq.sumBy (fun crab -> (crab - position) |> abs)

positions
    |> Seq.map cost1
    |> Seq.min
    |> printfn "%A"

// Part 2
let triangle n = (n*(n + 1))/2

let cost2 position =
    crabs |> Seq.sumBy (fun crab -> (crab - position) |> abs |> triangle)

positions
    |> Seq.map cost2
    |> Seq.min
    |> printfn "%A"