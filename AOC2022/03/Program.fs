let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\03\input"

let duplicate xs ys =
    let s1 = xs |> Set.ofSeq
    let s2 = ys |> Set.ofSeq
    Set.intersect s1 s2 |> Set.maxElement

let priority (c: char) =
    if System.Char.IsLower c then
        int c - 96
    else
        int c - 38

let p1 =
    System.IO.File.ReadAllLines(path)
    |> Array.map (fun row ->
        let s = row |> Seq.splitInto 2 |> Seq.toArray
        duplicate s[0] s[1])
    |> Array.map priority
    |> Array.sum
