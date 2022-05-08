let rand () = System.Random.Shared.NextDouble()
let randRange min max = rand () * (max - min) + min
let binarySearch target xs = System.Array.BinarySearch (xs, target) |> abs
let mutatePower () = randRange -10.0 10.0
let mutateOdds () = rand () < 0.1

type Agent = { data: float array; fitness: float }

let min, max = 0.0, 10.0

let randomFloatArray size =
    Array.init size (fun _ -> randRange min max)

let a1 = { data = randomFloatArray 10; fitness = 10.0 } 
let a2 = { data = randomFloatArray 10; fitness = 20.0 }
let a3 = { data = randomFloatArray 10; fitness = 30.0 }
//[10; 30; 60]

let agents = [|a1; a2; a3|]

let createWheel agents =
    let mutable s = 0.0
    agents |> Array.map (fun agent ->
        s <- s + agent.fitness
        s)

let wheel = createWheel agents

let pickIndex wheel =
    let roll = randRange 0.0 (wheel |> Array.last)
    binarySearch roll wheel - 1

let pick xs wheel =
    Array.get xs (pickIndex wheel)

let mutate agent =
    for i in 0..(agent.data |> Array.length) - 1 do
        if mutateOdds () then
            agent.data[i] <- agent.data[i] + mutatePower ()

