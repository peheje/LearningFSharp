let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\03\input"
let rows = System.IO.File.ReadAllLines(path)

let duplicate xs ys =
    let s1 = xs |> Set.ofSeq
    let s2 = ys |> Set.ofSeq
    Set.intersect s1 s2 |> Set.maxElement

let duplicate3 xs ys zs =
    let s1 = xs |> Set.ofSeq
    let s2 = ys |> Set.ofSeq
    let s3 = zs |> Set.ofSeq

    (Set.intersect s1 s2)
    |> Set.intersect s3
    |> Set.maxElement

let priority (c: char) =
    if System.Char.IsLower c then
        int c - 96
    else
        int c - 38

let p1 =
    rows
    |> Array.map (fun row ->
        let s = row |> Seq.splitInto 2 |> Seq.toArray
        duplicate s[0] s[1])
    |> Array.map priority
    |> Array.sum


let p2 =
    rows
    |> Array.chunkBySize 3
    |> Array.map (fun groups ->
        duplicate3 groups[0] groups[1] groups[2]
        |> priority)
    |> Array.sum
