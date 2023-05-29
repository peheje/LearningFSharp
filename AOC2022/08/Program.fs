let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\08\data.txt"

let xxs =
    System.IO.File.ReadAllLines path
    |> Array.map Array.ofSeq
    |> Array.map (Array.map (System.Char.GetNumericValue >> int))

let moveUp x y =
    [| for i in y .. -1 .. 0 -> xxs[i][x] |]

let moveDown x y =
    [| for i in y .. xxs.Length - 1 -> xxs[i][x] |]

let moveLeft x y =
    [| for i in x .. -1 .. 0 -> xxs[y][i] |]

let moveRight x y =
    [| for i in x .. xxs[0].Length - 1 -> xxs[y][i] |]

let scenic treesInDirection x y =
    let origin = xxs[y][x]

    treesInDirection x y
    |> Array.skip 1
    |> Array.fold
        (fun (stop, count) tree ->
            match (stop, tree) with
            | true, _ -> (true, count)
            | _, v when v >= origin -> (true, count + 1)
            | _ -> (false, count + 1))
        (false, 0)
    |> snd

let score x y =
    (scenic moveUp x y)
    * (scenic moveDown x y)
    * (scenic moveLeft x y)
    * (scenic moveRight x y)

let scores =
    xxs
    |> Array.mapi (fun y vs -> vs |> Array.mapi (fun x _ -> score x y))
    |> Array.collect id
    |> Array.max
