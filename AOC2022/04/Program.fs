let fullyContains (a, b) =
    let x = a |> Set.ofList
    let y = b |> Set.ofList
    Set.isSubset x y || Set.isSubset y x

let containsAny (a, b) =
    let x = a |> Set.ofList
    let y = b |> Set.ofList
    Set.intersect x y |> Set.isEmpty |> not

let range (section: string) =
    let s = section.Split("-")
    let start = s[0] |> int
    let stop = s[1] |> int
    [ start..stop ]

let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\04\input"
let raw = System.IO.File.ReadAllLines path

let ranges =
    raw
    |> Array.map (fun row ->
        let s = row.Split(",")
        let e1 = s[0] |> range
        let e2 = s[1] |> range
        e1, e2)

let p1 =
    ranges
    |> Array.map fullyContains
    |> Array.filter id
    |> Array.length

let p2 =
    ranges
    |> Array.map containsAny
    |> Array.filter id
    |> Array.length
