let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s
let split c a = (a:string).Split(c:string)
let sorted (s: string) = s |> Seq.sort |> Seq.map string |> String.concat ""

let input = System.IO.File.ReadAllLines "8.txt"

let parse line =
    let splitted = line |> split " | " |> Array.map (fun x -> x |> split " " |> Array.map sorted)
    (splitted[0], splitted[1])

let parsedInput = input |> Array.map parse

// Part 1
let part1 =
    parsedInput
    |> Array.map (fun (_, b) -> b)
    |> Array.collect id
    |> Array.map (fun x -> x |> String.length)
    |> Array.filter (fun c -> [|2;4;3;7|] |> Array.contains c)
    |> Array.length
    |> logs

// Part 2
let deduce (patterns: string array) =

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

    let three = hasLength 5 |> Seq.filter (hasInCommonWith one 2) |> Seq.exactlyOne

    let nine = hasLength 6 |> Seq.filter (hasInCommonWith three 5) |> Seq.exactlyOne

    let five =
        hasLength 5 
        |> Seq.filter (hasInCommonWith seven 2)
        |> Seq.filter (hasInCommonWith nine 5)
        |> Seq.exactlyOne

    let zero =
        hasLength 6
        |> Seq.filter (hasInCommonWith four 3)
        |> Seq.filter (hasInCommonWith seven 3)
        |> Seq.exactlyOne

    let two =
        hasLength 5
        |> Seq.filter (hasInCommonWith three 4)
        |> Seq.filter (hasInCommonWith five 3)
        |> Seq.exactlyOne

    let six =
        hasLength 6
        |> Seq.filter (hasInCommonWith seven 2)
        |> Seq.filter (hasInCommonWith nine 5)
        |> Seq.exactlyOne

    [|
        (zero, 0);
        (one, 1);
        (two, 2);
        (three, 3);
        (four, 4);
        (five, 5);
        (six, 6);
        (seven, 7);
        (eight, 8);
        (nine, 9);
    |] |> Array.map (fun (letters, number) -> (letters |> sorted, number))

let decode mappings code =
    let map = mappings |> Map.ofArray 
    code |> Array.map (fun x -> map |> Map.find x)

let intArrayToNumber xs =
    xs |> Array.map string |> String.concat "" |> int

let part2 =
    parsedInput
    |> Array.map (fun (a, b) -> (deduce a, b))
    |> Array.map (fun (a, b) -> decode a b)
    |> Array.map intArrayToNumber
    |> debugs
