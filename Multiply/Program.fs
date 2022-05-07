open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

let even x = x % 2 <> 0

let multiply1 a b =
    let mutable result = 0
    let mutable left = a
    let mutable right = b
    while left <> 0 do
        if even left then
            result <- result + right
        left <- left / 2
        right <- right * 2
    result

let multiply2 a b =
    let rec loop left right result =
        let nextLeft = left / 2
        let nextRight = right * 2
        if left = 0 then
            result
        elif even left then
            loop nextLeft nextRight (result + right)
        else
            loop nextLeft nextRight result
    loop a b 0

[<MemoryDiagnoser>]
[<ShortRunJob>]
type MultiplyComparison () =
    [<Benchmark>]
    member self.MutableMutate () = multiply1 234567 245

    [<Benchmark>]
    member self.RecursiveLoop () = multiply2 234567 245

let runner = BenchmarkRunner.Run<MultiplyComparison>()