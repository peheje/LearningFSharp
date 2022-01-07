let log x = printfn "%A" x
let reverse c = match c with | '[' -> ']' | '(' -> ')' | '{' -> '}' | '<' -> '>' | _ -> failwith "unknown reverse"
let point1 c = match c with | ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137 | _ -> failwith "unknown point"
let point2 c = match c with | ')' -> 1 | ']' -> 2 | '}' -> 3 | '>' -> 4 | _ -> failwith "unknown"
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

let input = System.IO.File.ReadAllLines "data.txt"

// Part 1
input
|> Seq.map (findUnexpected >> fst)
|> Seq.choose id
|> Seq.map point1
|> Seq.sum
|> log

// Part 2
input
|> Seq.map findUnexpected
|> Seq.filter (fst >> Option.isNone)
|> Seq.toArray
|> log