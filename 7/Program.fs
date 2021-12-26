let split c s = (s: string).Split(c: char)
let crabs = System.IO.File.ReadAllText("7.txt") |> split ',' |> Seq.map int
let positions = [0..crabs |> Seq.max]

let cost position =
    crabs |> Seq.sumBy (fun crab -> abs(crab - position))

positions
    |> Seq.map cost
    |> Seq.min
    |> printfn "%A"