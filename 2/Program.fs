let logs x = printfn "%A" x; x
let split (s: string) =
    let ss = s.Split " "
    (ss[0], int ss[1])

// Part 1
let add2 (a1, a2) (b1, b2) = a1 + b1, a2 + b2
let getActions (s: string) =
    match split s with
    | "forward", d -> (d, 0)
    | "down", d -> (0, d)
    | "up", d -> (0, -d)
let pos, depth =
    System.IO.File.ReadAllLines "2.txt"
    |> Seq.map getActions
    |> Seq.fold add2 (0, 0)
let result1 = pos * depth

// Part 2
type State = {pos: int; depth: int; aim: int}
let add s1 s2 = {pos = s1.pos + s2.pos; depth = s1.depth + s2.depth; aim = s1.aim + s2.aim}
let nextAction aim command =
    match command with
    | "forward", d -> {pos = d; depth = aim*d; aim = 0}
    | "down", d -> {pos = 0; depth = 0; aim = d}
    | "up", d -> {pos = 0; depth = 0; aim = -d}

let xs =
    System.IO.File.ReadAllLines "2.txt"
    |> Seq.map split
    |> Seq.fold (fun state command ->
        add (nextAction state.aim command) state
    ) {pos = 0;depth = 0; aim = 0}
    |> logs
let result2 = xs.depth * xs.pos