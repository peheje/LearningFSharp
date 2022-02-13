// Calculate the std deviation of a list
// The standard deviation is computed by taking the square root of the
// sum of the variances, which are the differences between each value
// and the average.

let myFloats = [2; 4; 4; 4; 5; 5; 7; 9] |> List.map float

let mean = myFloats |> List.average

let deviations = myFloats |> List.map (fun x -> (mean - x) ** 2)

let variance = deviations |> List.average

let stdDev = sqrt variance