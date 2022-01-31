let generator (first, second) =
    let nextFibNumber = first + second
    let nextState = (second, nextFibNumber)
    if nextFibNumber > 1000 then None
    else Some (nextFibNumber, nextState)

let fibSequence = Seq.unfold generator (1, 1)

for x in fibSequence do
    printfn "%d" x
