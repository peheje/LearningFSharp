let log x = printfn "%A" x

let remove a s = (s:string).Replace(a, "")
let s = "(((((((((())))))))))"

let opposite = Map [('{', '}'); ('<', '>'); ('[', ']'); ('(', ')')]
let opposite2 = Seq.zip (opposite |> Map.values) (opposite |> Map.keys) |> Map.ofSeq

let isCloser c = "})]>" |> Seq.contains c
let isOpener c = not (isCloser c)

let corrupted
let mutable ms = s
for i in 0 .. Seq.length s do
    ms <- ms |> remove "()"


log ms