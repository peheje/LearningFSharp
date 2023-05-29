module Part1

let parseRow xs =
    xs |> Seq.map (string >> System.Int32.Parse) |> Seq.toArray

let seen xs =
    ((-1, Set.empty), xs)
    ||> Array.fold (fun (max, coordinates) (value, coordinate) ->
        match value > max with
        | true -> (value, coordinates |> Set.add coordinate)
        | false -> (max, coordinates))
    |> snd

let path = "/Users/peterhelstrupjensen/repos/AOC2022_at_work/08/input"

let addIndices xxs =
    xxs |> Array.mapi (fun i xs -> xs |> Array.mapi (fun j x -> (x, (i, j))))

let rows1 = System.IO.File.ReadAllLines(path) |> Array.map parseRow |> addIndices
let rows2 = rows1 |> Array.map Array.rev
let cols1 = rows1 |> Array.transpose
let cols2 = cols1 |> Array.map Array.rev

let s1 = (rows1 |> Array.map seen) |> Set.unionMany
let s2 = (rows2 |> Array.map seen) |> Set.unionMany
let s3 = (cols1 |> Array.map seen) |> Set.unionMany
let s4 = (cols2 |> Array.map seen) |> Set.unionMany

let part1 = Set.unionMany [s1; s2; s3; s4] |> Set.count