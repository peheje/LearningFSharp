let split (c: string) (x: string) = x.Split(c)

let logs x =
    printfn "%A" x
    x

let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\08\data.txt"
let inp = System.IO.File.ReadAllLines path

let xxs =
    inp
    |> Array.map Array.ofSeq
    |> Array.map (fun xs ->
        xs
        |> Array.map (fun x -> System.Char.GetNumericValue(x) |> int))

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
    let origin = xxs[y][x]
    let mutable stop = false
    let mutable count = 0

    for x in xs do
        if stop then
            stop <- true
        elif x >= origin then
            count <- count + 1
            stop <- true
        else
            count <- count + 1

    count

let scenicUp x y = scenic moveUp x y
let scenicLeft x y = scenic moveLeft x y
let scenicRight x y = scenic moveRight x y
let scenicDown x y = scenic moveDown x y

scenicUp 2 1 |> printfn "%A" // should be 1
scenicLeft 2 1 |> printfn "%A" // should be 1
scenicDown 2 1 |> printfn "%A" // should be 2
scenicRight 2 1 |> printfn "%A" // should be 2

let scores =
    xxs
    |> Array.mapi (fun y vs ->
        vs
        |> Array.mapi (fun x _ ->
            (scenicUp x y)
            * (scenicDown x y)
            * (scenicLeft x y)
            * (scenicRight x y)))
    |> Array.collect id
    |> Array.max