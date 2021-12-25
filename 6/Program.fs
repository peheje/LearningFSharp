let log x = printfn $"%A{x}"
let split c s = (s: string).Split(c: char)
let input = "2,1,1,1,1,1,1,5,1,1,1,1,5,1,1,3,5,1,1,3,1,1,3,1,4,4,4,5,1,1,1,3,1,3,1,1,2,2,1,1,1,5,1,1,1,5,2,5,1,1,2,1,3,3,5,1,1,4,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,4,1,5,1,2,1,1,1,1,5,1,1,1,1,1,5,1,1,1,4,5,1,1,3,4,1,1,1,3,5,1,1,1,2,1,1,4,1,4,1,2,1,1,2,1,5,1,1,1,5,1,2,2,1,1,1,5,1,2,3,1,1,1,5,3,2,1,1,3,1,1,3,1,3,1,1,1,5,1,1,1,1,1,1,1,3,1,1,1,1,3,1,1,4,1,1,3,2,1,2,1,1,2,2,1,2,1,1,1,4,1,2,4,1,1,4,4,1,1,1,1,1,4,1,1,1,2,1,1,2,1,5,1,1,1,1,1,5,1,3,1,1,2,3,4,4,1,1,1,3,2,4,4,1,1,3,5,1,1,1,1,4,1,1,1,1,1,5,3,1,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,5,1,4,4,1,1,1,1,1,1,1,1,3,1,3,1,4,1,1,2,2,2,1,1,2,1,1"

let count =
    input
    |> split ','
    |> Array.map int
    |> Array.countBy id
    |> Map.ofArray

let mutable state =
    [| for i in 0 .. 8 ->
          match count |> Map.tryFind i with
          | Some v -> int64 v
          | _ -> 0 |]

let rotate l =
    match l |> Array.length with
    | 0 -> l
    | _ ->
        let first = l[0]
        let removed = l |> Array.removeAt 0
        Array.append removed [|first|]

for i in 1..256 do
    log i
    log state
    let reproducing = state[0]
    state <- state |> rotate
    state[6] <- state[6] + reproducing
    
state |> Array.sum |> log