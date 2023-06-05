let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\09\input.txt"
let log s v = printfn "%s %A" s v

let moves =
    System.IO.File.ReadAllLines(path)
    |> Array.map (fun row ->
        let s = row.Split(" ")
        let dir = s[0]
        let steps = int s[1]
        Array.create steps dir)
    |> Array.collect id
    |> Array.toList

let moveTail head tail =
    let (hx, hy) = head
    let (tx, ty) = tail
    let deltax = abs (hx - tx)
    let deltay = abs (hy - ty)
    deltax > 1 || deltay > 1

let newTailPosition head oldhead tail =
    if moveTail head tail then
        oldhead
    else
        tail

let rec move been tailAt headAt moves =
    let x, y = headAt

    match moves with
    | [] -> been
    | nextMove :: remainingMoves ->
        let newHeadAt =
            match nextMove with
            | "R" -> (x + 1, y)
            | "L" -> (x - 1, y)
            | "U" -> (x, y + 1)
            | "D" -> (x, y - 1)
            | _ -> failwith "unknown move"

        let newTailAt = newTailPosition newHeadAt headAt tailAt
        move (newTailAt :: been) newTailAt newHeadAt remainingMoves


move List.empty (0, 0) (0, 0) moves
|> List.distinct
|> List.length
|> printfn "%A"
