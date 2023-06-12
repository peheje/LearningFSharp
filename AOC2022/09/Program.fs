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

let rope = Array.init 10 (fun _ -> (0,0))

let add (hx, hy) (tx, ty) = (hx + tx, hy + ty)

let catchupMove head tail move =
    //printStep rope move

    let (hx, hy) = head
    let (tx, ty) = tail

    let xdif = hx - tx
    let ydif = hy - ty

    if abs xdif + abs ydif < 3 then
        // catch up in L, R, D, U
        if xdif > 1 then
            // move right
            (tx + 1, ty)
        elif xdif < -1 then
            // move left
            (tx - 1, ty)
        elif ydif < -1 then
            // move up
            (tx, ty - 1)
        elif ydif > 1 then
            // move down
            (tx, ty + 1)
        else
            tail
    else
        // catch up diagonally
        if ydif < 0 then
            // move up
            if xdif > 0 then
                // move right
                (tx + 1, ty - 1)
            else
                // move left
                (tx - 1, ty - 1)
        else
            // move down
            if xdif > 0 then
                // move right
                (tx + 1, ty + 1)
            else
                // move left
                (tx - 1, ty + 1)



printStep rope "initial state"
let visited = System.Collections.Generic.HashSet<(int*int)>()
for move in moves do
    let hx, hy = rope[0]
    match move with
    | "R" -> rope[0] <- hx + 1, hy
    | "L" -> rope[0] <- hx - 1, hy
    | "U" -> rope[0] <- hx, hy - 1
    | "D" -> rope[0] <- hx, hy + 1
    
    for i in 0..rope.Length-2 do
        rope[i+1] <- catchupMove rope[i] rope[i+1] move

    visited.Add rope[9] |> ignore

printfn "%i" visited.Count