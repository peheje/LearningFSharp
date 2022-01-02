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
    |> Array.map snd
    |> Array.collect id
    |> Array.map (fun x -> x |> String.length)
    |> Array.filter (fun c -> [|2;4;3;7|] |> Array.contains c)
    |> Array.length
    |> logs

// Part 2
let deduce patterns =
    let filterLength n = patterns |> Seq.filter (fun x -> x |> Seq.length = n)
    let hasInCommonWith b n a = a |> Seq.filter (fun c -> b |> Seq.contains c) |> Seq.length = n

    let one = filterLength 2 |> Seq.exactlyOne
    let four = filterLength 4 |> Seq.exactlyOne
    let seven = filterLength 3 |> Seq.exactlyOne
    let eight = filterLength 7 |> Seq.exactlyOne
    let three = filterLength 5 |> Seq.filter (hasInCommonWith one 2) |> Seq.exactlyOne
    let nine = filterLength 6 |> Seq.filter (hasInCommonWith three 5) |> Seq.exactlyOne

    let five =
        filterLength 5 
        |> Seq.filter (hasInCommonWith seven 2)
        |> Seq.filter (hasInCommonWith nine 5)
        |> Seq.exactlyOne

    let zero =
        filterLength 6
        |> Seq.filter (hasInCommonWith four 3)
        |> Seq.filter (hasInCommonWith seven 3)
        |> Seq.exactlyOne

    let two =
        filterLength 5
        |> Seq.filter (hasInCommonWith three 4)
        |> Seq.filter (hasInCommonWith five 3)
        |> Seq.exactlyOne

    let six =
        filterLength 6
        |> Seq.filter (hasInCommonWith seven 2)
        |> Seq.filter (hasInCommonWith nine 5)
        |> Seq.exactlyOne

    [| zero; one; two; three; four; five; six; seven; eight; nine |]
        |> Array.mapi (fun i x -> (x |> sorted, i))

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
    |> Array.sum
    |> logs