let rand () = System.Random.Shared.NextDouble()
let randRange min max = rand () * (max - min) + min
let binarySearch target xs = System.Array.BinarySearch (xs, target) |> abs
let mutatePower () = randRange 0.0 5.0
let mutateOdds () = rand () < 0.1
let poolSize = 1000

let crossoverPower () = rand () < 0.1
let crossoverOdds () = rand () < 0.1

type Agent = { data: float array; fitness: float }

let min, max = 0.0, 10.0
let target = [|0.0; 0.0; 1.0; 1.0; 2.0; 2.0; 3.0; 3.0; 4.0; 4.0; 5.0; 5.0; 6.0; 6.0; 7.0; 7.0; 8.0; 8.0; 9.0; 9.0|]
let argsize = target |> Array.length

let calculateFitness (data: float array) =
    let mutable fit = 0.0
    for i in 0..argsize-1 do
        fit <- fit + ((data[i] - target[i]) |> abs)
    1.0 / fit

let createAgent () =
    let data = Array.init argsize (fun _ -> randRange min max)
    { data = data; fitness = calculateFitness data }

let createWheel agents =
    let mutable s = 0.0
    agents |> Array.map (fun agent ->
        s <- s + agent.fitness
        s)

let pickIndex wheel =
    let roll = randRange 0.0 (wheel |> Array.last)
    binarySearch roll wheel - 1

let pick xs wheel =
    Array.get xs (pickIndex wheel)

let mutate agent =
    for i in 0..argsize-1 do
        if mutateOdds () then
            agent.data[i] <- agent.data[i] + mutatePower ()

let crossover agent pool wheel =
    let mate = pick pool wheel
    for i in 0..argsize-1 do
        if crossoverPower () then
            let tmp = agent.data[i]
            agent.data[i] <- mate.data[i]
            mate.data[i] <- tmp

let pool = Array.init poolSize (fun _ -> createAgent ())