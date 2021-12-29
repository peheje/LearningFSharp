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
let unknownSegment = -1

let lengthToNumberOrUnknown length =
    uniqueSegmentCountToNumber |> Map.tryFind length |> Option.defaultValue unknownSegment

type Wire = {number: int; source: string}

let exclusivePairwise xs ys =
    let rotate xs = Seq.append (xs |> Seq.removeAt 0) [(xs |> Seq.item 0)]
    let rec rotator xs = seq {
        yield xs
        yield! xs |> rotate |> rotator
    }
    xs |> rotator |> Seq.map (fun xs -> xs |> Seq.zip ys) |> Seq.take (xs |> Seq.length)

let rec keepFiltering predicate xs = seq {
    yield xs
    yield! xs |> predicate |> keepFiltering predicate
}

let mapper xs =
    let segments = 
        xs
        |> Array.map (fun x -> (x |> String.length, x))
        |> Array.map (fun (l, x) -> (lengthToNumberOrUnknown l, x))
        |> Array.map (fun (n, s) -> {number=n; source=s})
    
    let knowns =
        segments
        |> Array.filter (fun wire -> wire.number <> unknownSegment)
        |> Array.map (fun wire ->
            exclusivePairwise segmentMap[wire.number] wire.source
        )
        |> debugs

    segments

let part2 =
    input
    |> Array.map (fun x -> x |> split " | " |> Array.item 0 |> split " ")
    |> Array.map mapper


