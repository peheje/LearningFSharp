open Problems

let sw = System.Diagnostics.Stopwatch.StartNew()

let rand () = System.Random.Shared.NextDouble()
let randRange min max = rand () * (max - min) + min

type Agent = { xs: float array; score: float }

let sample agents =
    let i = System.Random.Shared.Next(agents |> Array.length)
    (agents[i]).xs

let print = 1000
let optimizer = rastrigin
let generations = 10000
let argsize = 100
let popsize = 200
let min, max = -5.12, 5.12
let clamp x = System.Math.Clamp(x, min, max)
let mutateRange () = randRange 0.2 0.95
let crossoverRange () = randRange 0.1 1.0

let createAgent () =
    let xs = Array.init argsize (fun _ -> randRange min max)
    { xs = xs; score = optimizer xs }

let pool = Array.init popsize (fun _ -> createAgent ())

let mate pool agent =
    let crossover = crossoverRange ()
    let mutate = mutateRange ()
    let x0 = pool |> sample
    let x1 = pool |> sample
    let x2 = pool |> sample

    let trial =
        Array.init argsize (fun j ->
            if rand () < crossover then
                (x0[j] + (x1[j] - x2[j]) * mutate) |> clamp
            else
                agent.xs[j])

    let trialScore = optimizer trial

    if trialScore < agent.score then
        { xs = trial; score = trialScore }
    else
        agent

let rec loop generation pool =
    if generation % print = 0 then
        let scores = pool |> Array.map (fun agent -> agent.score)
        printfn "generation %i" generation
        printfn "generation mean %f" (scores |> Array.average)
        printfn "generation minimum %f" (scores |> Array.min)

    if generation = generations then
        pool
    else
        let next = pool |> Array.Parallel.map (mate pool)
        loop (generation + 1) next

let best =
    loop 0 pool
    |> Array.minBy (fun agent -> agent.score)

printfn "generation best %A" best
printfn "execution time %i ms" sw.ElapsedMilliseconds
