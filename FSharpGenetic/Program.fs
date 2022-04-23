let sw = System.Diagnostics.Stopwatch.StartNew()

let square x = x * x

// Solve systems of equations like, x + y = 6 and -3x + y = 2, but with twists like please make x > y
let equation (xs: float array) =
    let x = xs[0]
    let y = xs[1]
    let err = if x < y then 100.0 else 0.0
    abs (x + y - 6.0) + abs (-3.0 * x + y - 2.0) + err

// There are 36 heads and 100 legs, how many horses and jockeys are there? 14 and 22
let horsesAndJockeys (xs: float array) =
    let horses = xs[0]
    let jockeys = xs[1]
    let legs = horses * 4.0 + jockeys * 2.0
    let heads = horses + jockeys
    abs (36.0 - heads) + abs (100.0 - legs)

// booth(1.0, 3.0) = 0
let booth (xs: float array) =
    square (xs[0] + 2.0 * xs[1] - 7.0)
    + square (2.0 * xs[0] + xs[1] - 5.0)

let rastrigin (xs: float array) =
    let pi = System.Math.PI
    let n = xs |> Array.length |> float
    let a = 10.0

    let sum =
        xs
        |> Array.sumBy (fun x -> (square x) - (a * cos (2.0 * pi * x)))

    a * n + sum

// f1(0..) = 0
let f1 xs = xs |> Array.sumBy (fun x -> x * x)
let rand () = System.Random.Shared.NextDouble()
let randRange min max = rand () * (max - min) + min

type Agent = { xs: float array; score: float }

let sample xs =
    let i = System.Random.Shared.Next(xs |> Array.length)
    (xs[i]).xs

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

let live pool agent =
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

let rec generationLoop g pool =
    if g % print = 0 then
        let scores = pool |> Array.map (fun agent -> agent.score)
        printfn "generation %i" g
        printfn "generation mean %f" (scores |> Array.average)
        printfn "generation minimum %f" (scores |> Array.min)

    if g = generations then
        pool
    else
        let next = pool |> Array.Parallel.map (live pool)
        generationLoop (g + 1) next

let best =
    generationLoop 0 pool
    |> Array.minBy (fun agent -> agent.score)

printfn "generation best %A" best
printfn "execution time %i ms" sw.ElapsedMilliseconds
