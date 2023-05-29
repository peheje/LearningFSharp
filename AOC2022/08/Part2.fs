module Part2

let split (c: string) (x: string) = x.Split(c)

let inp =
    """30373
25512
65332
33549
35390"""

let xxs =
    inp
    |> split "\n"
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
    let tallest = xxs[y][x]
    let xs = moveFun x y

    let count =
        xs
        |> Array.takeWhile (fun x -> x < tallest)
        |> Array.length

    count


scenic moveUp 2 1 |> printfn "%A"
scenic moveDown 2 1 |> printfn "%A"
scenic moveLeft 2 1 |> printfn "%A"
