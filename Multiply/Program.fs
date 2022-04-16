let benchmark name code =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    code()
    let elapsed = sw.ElapsedMilliseconds
    printfn "CPU time [%s] %ims" name elapsed

let multiply2 a b =
    let mutable result = 0
    let mutable left = a
    let mutable right = b
    while left <> 0 do
        if left % 2 <> 0 then
            result <- result + right
        left <- left / 2
        right <- right * 2
    result

let multiply a b =
    let rec loop left right result =
        let nextLeft = left / 2
        let nextRight = right * 2
        if left = 0 then
            result
        elif left % 2 <> 0 then
            loop nextLeft nextRight (result + right)
        else
            loop nextLeft nextRight result
    loop a b

multiply 4678 231452 |> printfn "%A"

// = 1082732456

let size = 100_000_000
let mutable y = 0.0
benchmark "multiply" (fun () -> 
    for _ = 0 to size do
        y <- y + (multiply 4678 231452 |> float)
)
printfn "%f" y

y <- 0
benchmark "*" (fun () -> 
    for _ = 0 to size do
        y <- y + (4678 * 231452 |> float)
)
printfn "%f" y