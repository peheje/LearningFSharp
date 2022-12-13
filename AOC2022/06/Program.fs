let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/06/input"
let rows = System.IO.File.ReadAllLines path

let allUnique3 (xs: seq<'a>) =
    let seen = System.Collections.Generic.HashSet<'a>()
    use enumerator = xs.GetEnumerator()
    let mutable unique = true

    while unique && enumerator.MoveNext() do
        let current = enumerator.Current

        if seen.Contains current then
            unique <- false
        else
            seen.Add(current) |> ignore

    unique

let allUnique2 xs =
    let seen = System.Collections.Generic.HashSet<'a>()

    xs
    |> Seq.tryFind (fun x ->
        if seen.Contains x then
            true
        else
            seen.Add x |> ignore
            false)
    |> Option.isNone

let allUnique1 l =
    l |> Seq.distinct |> Seq.length = (l |> Seq.length)

let firstUnique size row =
    // Using Seq will only evaluate the required amount of windows
    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences#searching-and-finding-elements
    (row
     |> Seq.windowed size
     |> Seq.findIndex allUnique3)
    + size

let firstUnique4 = firstUnique 4
let firstUnique14 = firstUnique 14

let sw = System.Diagnostics.Stopwatch.StartNew()

let part1 = rows |> Seq.map firstUnique4 |> Seq.head
let part2 = rows |> Seq.map firstUnique14 |> Seq.head

sw.Stop()

match (part1, part2) with
| (1625, 2250) -> printfn "all ok"
| _ -> failwith "not correct"

printfn "part1 %i" part1
printfn "part2 %i" part2
printfn "elapsed %ims" sw.ElapsedMilliseconds
