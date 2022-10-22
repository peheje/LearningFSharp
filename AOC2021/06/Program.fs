let log x = printfn $"%A{x}"
let split c s = (s: string).Split(c: char)
let countWhere predicate = Seq.filter predicate >> Seq.length

let input =
    "2,1,1,1,1,1,1,5,1,1,1,1,5,1,1,3,5,1,1,3,1,1,3,1,4,4,4,5,1,1,1,3,1,3,1,1,2,2,1,1,1,5,1,1,1,5,2,5,1,1,2,1,3,3,5,1,1,4,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,4,1,5,1,2,1,1,1,1,5,1,1,1,1,1,5,1,1,1,4,5,1,1,3,4,1,1,1,3,5,1,1,1,2,1,1,4,1,4,1,2,1,1,2,1,5,1,1,1,5,1,2,2,1,1,1,5,1,2,3,1,1,1,5,3,2,1,1,3,1,1,3,1,3,1,1,1,5,1,1,1,1,1,1,1,3,1,1,1,1,3,1,1,4,1,1,3,2,1,2,1,1,2,2,1,2,1,1,1,4,1,2,4,1,1,4,4,1,1,1,1,1,4,1,1,1,2,1,1,2,1,5,1,1,1,1,1,5,1,3,1,1,2,3,4,4,1,1,1,3,2,4,4,1,1,3,5,1,1,1,1,4,1,1,1,1,1,5,3,1,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,5,1,4,4,1,1,1,1,1,1,1,1,3,1,3,1,4,1,1,2,2,2,1,1,2,1,1"
    |> split ','
    |> Seq.map int

let step xs =
    xs |> List.mapi (fun i _ ->
        match i with
        | 6 -> xs[7] + xs[0]    // those with 7 days left now has 6 days left plus those who will procreate and reset back to 6 
        | 8 -> xs[0]            // offspring of those who procreate
        | _ -> xs[i + 1])

let rec loop i stop xs =
    if i = stop then xs
    else xs |> step |> loop (i + 1) stop

let rec next xs = seq {         // instead of recursive function with state, plays nicely when number of iterations is given
    yield xs
    yield! xs |> step |> next
}

let sw = System.Diagnostics.Stopwatch.StartNew()

[0..8]
    |> List.map (fun i -> input |> countWhere (fun x -> x = i) |> int64)
    |> next |> Seq.item 256
    // |> loop 0 256
    |> List.sum
    |> log

printfn "Took %ims" sw.ElapsedMilliseconds