// Calculate sum of elements in list using fold
let myInts = [1;2;3;4;5] // sum is 15
let mySum = List.fold(fun acc item -> acc + item) 0 myInts

// Calculate the std deviation of a list
// The standard deviation is computed by taking the square root of the
// sum of the variances, which are the differences between each value
// and the average.

let myFloats = [1.0; 2.0; 3.0; 4.0; 5.0]
let avg = myFloats |> Seq.average

let variance = 
    Seq.fold (fun acc item -> 
        acc + (((avg - item) ** 2.0)) / float (myFloats |> List.length)
    ) 0.0 myFloats

let stdDev = sqrt variance