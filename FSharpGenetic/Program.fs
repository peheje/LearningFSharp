let f1 xs = xs |> Array.sumBy (fun x -> x*x)
let rand () = System.Random.Shared.NextDouble()
let randRange min max = rand() * (max - min) + min
let randomElement xs =
    Array.get xs (System.Random.Shared.Next(xs |> Array.length))
let log x = printfn "%A" x

let print = 1000
let optimizer = f1
let generations = 10000
let argsize = 400
let popsize = 400
let clamp x = System.Math.Clamp(x, -10.0, 10.0)
let mutatePowerRange () = randRange 0.2 0.95
let crossoverRiskRange () = randRange 0.1 1.0
let createAgent () = Array.init argsize (fun _ -> rand())
let mutable pop = Array.init popsize (fun _ -> 
    let agent = createAgent()
    let score = optimizer agent
    (agent, score)
)

for g in 0..generations-1 do
    let crossoverRisk = crossoverRiskRange()
    let crossover () = rand() < crossoverRisk
    let mutate = mutatePowerRange()

    pop <- pop |> Array.Parallel.map (fun (xt, xScore) ->
        let x0 = pop |> randomElement |> fst
        let x1 = pop |> randomElement |> fst
        let x2 = pop |> randomElement |> fst
        let trial = [|for j in 0..argsize-1 -> if crossover() then (x0[j] + (x1[j] - x2[j]) * mutate) |> clamp else xt[j]|]
        let trialScore = optimizer trial
        if trialScore < xScore then (trial, trialScore) else (xt, xScore)
    )

    if g % print = 0 then
        let scores = pop |> Array.map (fun (_, score) -> score)
        let best = Array.zip scores pop |> Array.minBy (fun (score, _) -> score) |> snd
        log $"generation best"
        log best
        log $"generation {g}"
        log $"generation mean {scores |> Array.average}"
        log $"generation best {scores |> Array.min}"