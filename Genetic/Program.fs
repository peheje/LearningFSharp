let rand () = System.Random.Shared.NextDouble()
let randRange min max = rand () * (max - min) + min
let binarySearch target xs = System.Array.BinarySearch (xs, target) |> abs
let mutatePower () = randRange -1.0 1.0
let mutateOdds () = rand () < 0.05
let crossoverPower () = rand () < 0.05
let crossoverOdds () = rand () < 0.05

type Agent = { data: float array; mutable fitness: float }

let poolSize = 1000
let generations = 100_000
let print = 10_000
let low, high = 0.0, 10.0
let clamp x = System.Math.Clamp(x, low, high)
//let target = [|0.0; 0.0; 1.0; 1.0; 2.0; 2.0; 3.0; 3.0; 4.0; 4.0; 5.0; 5.0; 6.0; 6.0; 7.0; 7.0; 8.0; 8.0; 9.0; 9.0|]
let target = [|1.0; 0.0|]
let argsize = target |> Array.length

let calculateFitness (data: float array) =
    let mutable totalError = 0.0
    for i in 0..argsize-1 do
        let value = data[i]
        let target = target[i]
        let error = (value - target) |> abs
        totalError <- totalError + error
    100.0 / totalError    

let createAgent () =
    let data = Array.init argsize (fun _ -> randRange low high)
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
    let idx = pickIndex wheel
    Array.get xs idx

let mutate agent =
    for i in 0..argsize-1 do
        if mutateOdds () then
            agent.data[i] <- (agent.data[i] + mutatePower ()) |> clamp

let crossover agent pool wheel =
    let mate = pick pool wheel
    for i in 0..argsize-1 do
        if crossoverPower () then
            let tmp = agent.data[i]
            agent.data[i] <- mate.data[i]
            mate.data[i] <- tmp

let mutable pool = Array.init poolSize (fun _ -> createAgent ())

for generation in 0..generations do
    let wheel = createWheel pool
    for agent in pool do
        if crossoverOdds () then crossover agent pool wheel
        if mutateOdds () then mutate agent
        agent.fitness <- calculateFitness agent.data
    pool <- Array.init poolSize (fun _ -> pick pool wheel)
    
    if generation % print = 0 || generation = generations then
        let best = pool |> Array.maxBy (fun a -> a.fitness)
        printfn "best agent is %A" best