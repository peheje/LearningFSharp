let fibSequence =
    (1, 1)
    |> Seq.unfold (fun (first, second) ->
        let nextFibNumber = first + second
        let nextState = (second, nextFibNumber)
        if nextFibNumber > 1000 then None
        else Some (nextFibNumber, nextState))

for x in fibSequence do
    printfn "%d" x

// Flip sequence [1; 0; 1; 0 ..]
let flipSequence =
    (0)
    |> Seq.unfold (fun current ->
        let next = if current = 0 then 1 else 0
        Some (next, next))

let flips = flipSequence |> Seq.take 10 |> Seq.toList

// Calculate sin(0.1) then cos(0.2) then sin(0.3) etc..
let sinCosSequence =
    (0.1, true)
    |> Seq.unfold (fun (current, useSin) ->
        let value = if useSin then sin(current) else cos(current)
        Some (value, (current + 0.1, not useSin)))

let sinCoses = sinCosSequence |> Seq.take 4 |> Seq.toList