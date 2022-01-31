let generator (first, second) =
    let nextFibNumber = first + second
    let nextState = (second, nextFibNumber)
    Some (nextFibNumber, nextState)

let fib =
    Seq.unfold generator (1, 1)
    |> Seq.take 10
    |> Seq.toList