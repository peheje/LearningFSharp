let rows = System.IO.File.ReadAllLines("C:\Users\peter\Repos\LearningFSharp\AOC2022\02\input")

type Hand = | Rock | Paper | Scissor
type Outcome = | Win | Loss | Draw

let outcome (theirs, yours) =
    match yours, theirs with
    | Rock, Scissor -> Win
    | Rock, Paper -> Loss
    | Paper, Rock -> Win
    | Paper, Scissor -> Loss
    | Scissor, Paper -> Win
    | Scissor, Rock -> Loss
    | _, _ -> Draw

let strategy1 = rows |> Array.map(fun row ->
    let s = row.Split(" ")
    
    let theyPlay =
        match s[0] with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissor
        | _ -> failwith "not abc"

    let wePlay =
        match s[1] with
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissor
        | _ -> failwith "not xyz"

    (theyPlay, wePlay)
)

let handScore = function
    | Rock -> 1 | Paper -> 2 | Scissor -> 3

let outcomeScore = function
    | Win -> 6 | Draw -> 3 | Loss -> 0

let outcomesScores1 = strategy1 |> Array.map (outcome >> outcomeScore)
let handScores1 = strategy1 |> Array.map (fun (_, wePlay) -> handScore wePlay)
let total1 = (outcomesScores1 |> Array.sum) + (handScores1 |> Array.sum)

let handForOutcome (theirHand, outcome) =
    match (theirHand, outcome) with
    | Rock, Win -> Paper
    | Rock, Loss -> Scissor
    | Paper, Win -> Scissor
    | Paper, Loss -> Rock
    | Scissor, Win -> Rock
    | Scissor, Loss -> Paper
    | _, _ -> theirHand

let strategy2 = rows |> Array.map(fun row ->
    let s = row.Split(" ")
    
    let theyPlay =
        match s[0] with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissor
        | _ -> failwith "not abc"

    let theOutcome =
        match s[1] with
        | "X" -> Loss
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> failwith "not xyz"

    (theyPlay, theOutcome)
)

let handScores2 = strategy2 |> Array.map (fun s -> (handForOutcome s) |> handScore)
let outcomeScores2 = strategy2 |> Array.map (fun (_, theOutcome) -> outcomeScore theOutcome)
let total2 = (handScores2 |> Array.sum) + (outcomeScores2 |> Array.sum)