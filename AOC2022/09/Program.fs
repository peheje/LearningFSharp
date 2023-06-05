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

let shouldMoveTail head tail =
    let (hx, hy) = head
    let (tx, ty) = tail
    let deltax = abs (hx - tx)
    let deltay = abs (hy - ty)
    deltax > 1 || deltay > 1

let newTailPosition head previousHead tail =
    if shouldMoveTail head tail then
        previousHead
    else
        tail

let rec move tailBeen tailAt headAt moves =
    let x, y = headAt

    match moves with
    | [] -> tailBeen
    | nextMove :: remainingMoves ->
        let newHeadAt =
            match nextMove with
            | "R" -> (x + 1, y)
            | "L" -> (x - 1, y)
            | "U" -> (x, y + 1)
            | "D" -> (x, y - 1)
            | _ -> failwith "unknown move"

        let newTailAt = newTailPosition newHeadAt headAt tailAt
        move (newTailAt :: tailBeen) newTailAt newHeadAt remainingMoves


move List.empty (0, 0) (0, 0) moves
|> List.distinct
|> List.length
|> printfn "%A"
