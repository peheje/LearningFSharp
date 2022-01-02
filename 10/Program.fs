let log x = printfn "%A" x
let isStarter c = ['['; '('; '{'; '<'] |> Seq.contains c
let reverse c = match c with | '[' -> ']' | '(' -> ')' | '{' -> '}' | '<' -> '>' | _ -> failwith "unknown character"

let rec loop stack xs =
    match xs with
    | [] -> None
    | x::rest ->
        if isStarter x then
            loop (x::stack) rest
        else
            match stack with
            | [] -> loop stack rest
            | s::sx -> if reverse s <> x then Some x else loop sx rest


let findUnexpected xs = loop List.empty (xs |> Seq.toList)

findUnexpected "[<>({}){}[([])<>]]" |> log
findUnexpected "(((((((((())))))))))" |> log
findUnexpected "{([(<{}[<>[]}>{[]{[(<()>" |> log
findUnexpected "[[<[([]))<([[{}[[()]]]" |> log