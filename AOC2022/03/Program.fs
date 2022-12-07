let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\03\input"
let raw = System.IO.File.ReadAllLines(path)

let duplicate xs ys =
    let s1 = xs |> Set.ofSeq
    let s2 = ys |> Set.ofSeq
    Set.intersect s1 s2 |> Set.maxElement

let intersects = raw |> Array.map (fun row ->
    let s = row |> Seq.splitInto 2 |> Seq.toArray
    duplicate s[0] s[1]
)

let priority (c: char) =
    if System.Char.IsLower c then
        int c - 96
    else
        int c - 38

intersects |> Array.map priority |> Array.sum