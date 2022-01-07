let logs x = printfn "%A" x; x
let reverse c = match c with | '[' -> ']' | '(' -> ')' | '{' -> '}' | '<' -> '>' | _ -> failwith "unknown reverse"
let point1 c = match c with | ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137 | _ -> failwith "unknown point"
let isStarter c = ['['; '('; '{'; '<'] |> Seq.contains c

let findUnexpected xs =
    let rec loop stack xs =
        match xs with
        | [] -> (None, stack)
        | x::rest ->
            if isStarter x then
                loop (reverse x::stack) rest
            else
                match stack with
                | [] -> (Some x, stack)
                | s::sx -> if s = x then loop sx rest else (Some x, stack)
    loop List.empty (xs |> Seq.toList)

let input = System.IO.File.ReadAllLines "sample.txt"

// Part 1
let part1 = 
    input
    |> Seq.map (findUnexpected >> fst)
    |> Seq.choose id
    |> Seq.map point1
    |> Seq.sum
    |> logs

// Part 2
let point2 c = match c with | ')' -> 1 | ']' -> 2 | '}' -> 3 | '>' -> 4 | _ -> failwith "unknown"

let score xs =
    xs
    |> Seq.map point2
    |> Seq.fold (fun acc x -> acc * 5 + x) 0

let part2 =
    input
    |> Seq.map findUnexpected
    |> Seq.filter (fst >> Option.isNone)
    |> Seq.map (snd >> score)
    |> Seq.sort
    |> Seq.splitInto 2
    |> Seq.item 0
    |> Seq.last
    |> logs
