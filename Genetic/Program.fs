let rand () = System.Random.Shared.NextDouble()
let randRange min max = rand () * (max - min) + min
let binarySearch target xs = System.Array.BinarySearch (xs, target) |> abs
let mutatePower () = randRange -10.0 10.0
let mutateOdds () = rand () < 0.1

let crossoverPower () = rand () < 0.1
let crossoverOdds () = rand () < 0.1

type Agent = { data: float array; fitness: float }

let min, max = 0.0, 10.0

let argsize = 10

let randomFloatArray size =
    Array.init size (fun _ -> randRange min max)

let a1 = { data = randomFloatArray argsize; fitness = 10.0 } 
let a2 = { data = randomFloatArray argsize; fitness = 20.0 }
let a3 = { data = randomFloatArray argsize; fitness = 30.0 }
//[10; 30; 60]

let agents = [|a1; a2; a3|]

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
