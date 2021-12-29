let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s

let split c a = (a:string).Split(c:string)

let input = System.IO.File.ReadAllLines "8.txt"

// Part 1
let part1 =
    input
    |> Array.map (fun x -> x |> split " | " |> Array.item 1 |> split " ")
    |> Array.collect id
    |> Array.map (fun x -> x |> String.length)
    |> Array.filter (fun c -> [|2;4;3;7|] |> Array.contains c)
    |> Array.length
    |> logs

// Part 2
let deduce (patterns:string array) =

    let hasUniqueLength n =
        patterns |> Seq.find (fun pattern -> pattern |> Seq.length = n)

    let one = hasUniqueLength 2
    let four = hasUniqueLength 4
    let seven = hasUniqueLength 3
    let eight = hasUniqueLength 7

    let hasLength n =
        patterns |> Seq.filter (fun pattern -> pattern |> Seq.length = n)

    let hasInCommonWith b n a =
        a |> Seq.filter (fun c -> b |> Seq.contains c) |> Seq.length = n

    let five = hasLength 5 |> Seq.find (hasInCommonWith four 3)

    let zero = hasLength 6 |> Seq.find (hasInCommonWith five 4)

    let two = "todo"

    let three = "todo"

    let six = "todo"

    let nine = "todo"

    [
        (0, zero);
        (1, one);
        (2, two);
        (3, three);
        (4, four);
        (5, five);
        (6, six);
        (7, seven);
        (8, eight);
        (9, nine);
    ]

let part2 =
    input
    |> Array.map (fun x -> x |> split " | " |> Array.item 0 |> split " ")
    |> Array.map deduce
    |> debugs

