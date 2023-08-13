let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/09/input.txt"

let sw = System.Diagnostics.Stopwatch.StartNew()

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

let moveRight (x, y) = x + 1, y
let moveLeft (x, y) = x - 1, y
let moveUp (x, y) = x, y - 1
let moveDown (x, y) = x, y + 1

let catchupMove head tail =
    let (hx, hy) = head
    let (tx, ty) = tail
    let xdif = hx - tx
    let ydif = hy - ty

    if abs xdif + abs ydif < 3 then
        // catch up in L, R, D, U or not
        match xdif, ydif with
        | x, _ when x > 1 -> moveRight tail
        | x, _ when x < -1 -> moveLeft tail
        | _, y when y < -1 -> moveUp tail
        | _, y when y > 1 -> moveDown tail
        | _, _ -> tail
    else
        // catch up diagonally
        let upOrDown = if ydif < 0 then moveUp else moveDown
        let leftOrRight = if xdif > 0 then moveRight else moveLeft
        (upOrDown >> leftOrRight) tail


seq {
    let rope = Array.init 10 (fun _ -> (0, 0))

    for move in moves do
        let hx, hy = rope[0]

        match move with
        | "R" -> rope[0] <- hx + 1, hy
        | "L" -> rope[0] <- hx - 1, hy
        | "U" -> rope[0] <- hx, hy - 1
        | "D" -> rope[0] <- hx, hy + 1

        for i in 0 .. rope.Length - 2 do
            rope[i + 1] <- catchupMove rope[i] rope[i + 1]

        rope[9]
}
|> Seq.distinct
|> Seq.length
|> printfn "%i"

sw.Stop()

printfn "Elapsed time %ims" sw.ElapsedMilliseconds