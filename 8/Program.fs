let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug1.json", json s); s

let numberToSegmentCount = Map [
    (0, 6); (1, 2); (2, 5); (3, 5); (4, 4); (5, 5); (6, 6); (7, 3); (8, 7); (9, 6)
]

let uniqueSegmentCountToNumber = Map [
    (2, 1); (4, 4); (3, 7); (7, 8)
]

let uniqueSegmentCounts = uniqueSegmentCountToNumber |> Map.keys |> Seq.toArray

let split c a = (a:string).Split(c:string)

let input = System.IO.File.ReadAllLines "sample.txt"

let part1 =
    input
    |> Array.map (fun x -> x |> split " | " |> Array.item 1 |> split " ")
    |> Array.collect id
    |> Array.map (fun x -> x |> String.length)
    |> Array.filter (fun c -> uniqueSegmentCounts |> Array.contains c)
    |> Array.length

let unknownSegment = -1

let lengthToNumberOrUnknown length =
    uniqueSegmentCountToNumber |> Map.tryFind length |> Option.defaultValue unknownSegment

type Wire = {number: int; length: int; source: string}

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

let mapper xs =
    let segments = 
        xs
        |> Array.map (fun x -> (x |> String.length, x))
        |> Array.map (fun (l, x) -> (lengthToNumberOrUnknown l, l, x))
        |> Array.map (fun (n, l, s) -> {number=n; length=l; source=s})
        |> Array.sortBy (fun wire -> wire.length)
    
    let known =
        segments
        |> Array.filter (fun wire -> wire.number <> unknownSegment)
        |> Array.map (fun wire -> 
            Array.allPairs (wire.source |> Seq.toArray) segmentMap[wire.number]
        )
        |> Array.collect id
        |> Array.groupBy (fun (segmentNumber, number) -> number)
        |> debugs

    segments

let part2 =
    input
    |> Array.map (fun x -> x |> split " | " |> Array.item 0 |> split " ")
    |> Array.map mapper

