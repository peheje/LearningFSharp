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

let printStep rope move =
    let maxx = rope |> Array.map fst |> Array.max
    let minx = rope |> Array.map fst |> Array.min
    let maxy = rope |> Array.map snd |> Array.max
    let miny = rope |> Array.map snd |> Array.min

    printfn "move %s" move
    let mincord, maxcord = -6, 6
    for y in mincord..maxcord do
        for x in mincord..maxcord do
            match rope |> Array.tryFindIndex (fun spot -> spot = (x, y)) with
            | Some v -> printf "%i " v
            | None -> printf ". "
        printfn ""
    printfn ""
    printfn ""


let isDetached head tail =
    let hx, hy = head
    let tx, ty = tail
    (abs (hx-tx) > 1) || (abs (hy-ty) > 1)

let rope = Array.init 2 (fun _ -> (0,0))

printStep rope "initial state"
let visited = System.Collections.Generic.HashSet<(int*int)>()
for move in moves do
    let hx, hy = rope[0]
    match move with
    | "R" ->
        rope[0] <- hx + 1, hy
        if isDetached rope[0] rope[1] then
            rope[1] <- hx, hy
    | "L" ->
        rope[0] <- hx - 1, hy
        if isDetached rope[0] rope[1] then
            rope[1] <- hx, hy
    | "U" ->
        rope[0] <- hx, hy - 1
        if isDetached rope[0] rope[1] then
            rope[1] <- hx, hy
    | "D" ->
        rope[0] <- hx, hy + 1
        if isDetached rope[0] rope[1] then
            rope[1] <- hx, hy
    visited.Add rope[1] |> ignore
    //printStep rope move

printfn "%i" visited.Count