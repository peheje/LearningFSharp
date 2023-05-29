let split (c: string) (x: string) = x.Split(c)

let logs x = printfn "%A" x; x

let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/08/data.txt"
let inp = System.IO.File.ReadAllLines path
let xxs =
    inp
    |> Array.map Array.ofSeq
    |> Array.map (fun xs -> xs |> Array.map (fun x -> System.Char.GetNumericValue(x) |> int))

let moveUp x y =
    [| for i in y .. -1 .. 0 -> xxs[i][x] |]

let moveDown x y =
    [| for i in y .. xxs.Length - 1 -> xxs[i][x] |]

let moveLeft x y =
    [| for i in x .. -1 .. 0 -> xxs[y][i] |]

let moveRight x y =
    [| for i in x .. xxs[0].Length - 1 -> xxs[y][i] |]

let scenic moveFun x y =
    let xs = moveFun x y |> Array.skip 1
    let myPosition = xxs[y][x]
    let mutable stop = false
    let mutable count = 0
    for x in xs do
        if stop then
            stop <- true
        elif x >= myPosition then
            count <- count + 1
            stop <- true
        else
            count <- count + 1
    count

scenic moveUp 2 1 |> printfn "%A" // should be 1
scenic moveLeft 2 1 |> printfn "%A" // should be 1
scenic moveDown 2 1 |> printfn "%A" // should be 2
scenic moveRight 2 1 |> printfn "%A" // should be 2