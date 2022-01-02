let log x = printfn "%A" x

let s = "(((((((((())))))))))" |> Seq.toArray

let opposite = Map [('{', '}'); ('<', '>'); ('[', ']'); ('(', ')')]
let opposite2 = Seq.zip (opposite |> Map.values) (opposite |> Map.keys) |> Map.ofSeq

let isCloser c = "})]>" |> Seq.contains c
let isOpener c = not (isCloser c)

s |> Array.fold (fun acc c ->
    log acc
    if isOpener c then
        opposite[c]::acc
    else
        match acc with
        | x::xs -> if opposite2[c] <> x then failwith "unmatched1" else xs
        | [] -> failwith "unmatched2"

) []
|> log