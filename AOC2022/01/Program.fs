let path =
    "/Users/peterhelstrupjensen/repos/LearningFSharp/AOC2022/01/input"

let pt1 =
    System.IO.File.ReadAllText(path).Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.map int |> Array.sum)
    |> Array.max

let pt2 =
    System.IO.File.ReadAllText(path).Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.map int |> Array.sum)
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum