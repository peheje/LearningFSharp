let rows = System.IO.File.ReadAllLines("C:\Users\peter\Repos\LearningFSharp\AOC2022\02\input")

type Hand = | Rock | Paper | Scissor
type Outcome = | Win | Loss | Draw

let outcome (yours, theirs) =
    match yours, theirs with
    | Rock, Scissor -> Win
    | Rock, Paper -> Loss
    | Paper, Rock -> Win
    | Paper, Scissor -> Loss
    | Scissor, Paper -> Win
    | Scissor, Rock -> Loss
    | _, _ -> Draw

let plays = rows |> Array.map(fun row ->
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

    (wePlay, theyPlay)
)

let shapeScore = function
    | Rock -> 1 | Paper -> 2 | Scissor -> 3

let outcomeScore = function
    | Win -> 6 | Draw -> 3 | Loss -> 0

let outcomesScores = plays |> Array.map (outcome >> outcomeScore)
let shapeScores = plays |> Array.map (fun (wePlay, _) -> shapeScore wePlay)

let total = (outcomesScores |> Array.sum) + (shapeScores |> Array.sum)
