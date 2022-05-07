let min, max = 0.0, 10.0

let a = [| 1.0; 2.0; 0.0 |]
let b = [| 2.0; 4.0; 10.0 |]
let c = [| 3.0; 6.0; 20.0 |]

let crossoverRangeSmart xs =
    let range = abs (min - max)
    let diff (xs: float array) = abs (xs[0] - xs[1])
    let meanDiffs xs =
        xs
        |> Array.windowed 2
        |> Array.map diff
        |> Array.average

    xs
    |> Array.transpose
    |> Array.map meanDiffs
    |> Array.map (fun x -> x / range)
    |> Array.max

let xs = [| a; b; c |] |> crossoverRangeSmart