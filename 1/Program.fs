let log x = printfn "%A" x
let readInts path = System.IO.File.ReadAllLines path |> Seq.map int
let hasIncreased (a, b) = a < b
let countIncreased xs = xs |> Seq.pairwise |> Seq.filter hasIncreased |> Seq.length

// Part 1
readInts "1.txt"
|> countIncreased
|> log

// Part 2
readInts "1.txt" 
|> Seq.windowed 3
|> Seq.map Seq.sum
|> countIncreased
|> log