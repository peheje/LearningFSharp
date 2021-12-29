let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s

let numberToSegmentCount = Map [
    (0, 6); (1, 2); (2, 5); (3, 5); (4, 4); (5, 5); (6, 6); (7, 3); (8, 7); (9, 6)
]

let uniqueSegmentCountToNumber = Map [
    (2, 1); (4, 4); (3, 7); (7, 8)
]

let segmentMap = [|
    [|1;2;3;5;6;7|];
    [|3;6|];
    [|1;3;4;5;7|];
    [|1;3;4;6;7|];
    [|2;3;4;6;|];
    [|1;2;4;6;7|];
    [|1;2;4;5;6;7|];
    [|1;3;6|];
    [|1;2;3;4;5;6;7|];
    [|1;2;3;4;6;7|]
|]

let uniqueSegmentCounts = uniqueSegmentCountToNumber |> Map.keys |> Seq.toArray

let split c a = (a:string).Split(c:string)

let input = System.IO.File.ReadAllLines "sample.txt"

// Part 1

let part1 =
    input
    |> Array.map (fun x -> x |> split " | " |> Array.item 1 |> split " ")
    |> Array.collect id
    |> Array.map (fun x -> x |> String.length)
    |> Array.filter (fun c -> uniqueSegmentCounts |> Array.contains c)
    |> Array.length

// Part 2

let exclusivePairwise xs ys =
    let rotate xs = Seq.append (xs |> Seq.removeAt 0) [(xs |> Seq.item 0)]
    let rec rotator xs = seq {
        yield xs
        yield! xs |> rotate |> rotator
    }
    xs |> rotator |> Seq.map (fun xs -> xs |> Seq.zip ys) |> Seq.take (xs |> Seq.length)

let mapper xs =
    let segments = 
        xs
        |> Array.map (fun x -> (x |> String.length, x))
        |> Array.map (fun (l, x) -> (uniqueSegmentCountToNumber |> Map.tryFind l, x))
        |> Array.map (fun (n, x) -> (n, x, if Option.isSome n then segmentMap[n.Value] else [||]))
        |> Array.map (fun (n, x, s) -> (n, x, exclusivePairwise x s))
    
    let (knowns, unknowns) =
        segments
        |> Array.partition (fun (n, _, _) -> Option.isSome n)

    let rules =
        knowns
        |> Seq.map (fun (n, x, s) -> s)

    let flattenedRules = rules |> Seq.collect id |> Seq.rev

    let mutable rulesLeft = flattenedRules

    for rule in flattenedRules do
        for (number, character) in rule do
            rulesLeft <- rulesLeft |> Seq.filter (fun xs -> 
                xs |> Seq.exists (fun (i, c) -> i = number && c <> character)
            )

        logs rulesLeft



    segments

let part2 =
    input
    |> Array.map (fun x -> x |> split " | " |> Array.item 0 |> split " ")
    |> Array.map mapper

