type Octo = { row: int; col: int; value: int }

let octos =
    System.IO.File.ReadAllLines("data.txt")
    |> Seq.mapi (fun r row ->
        row
        |> Seq.mapi (fun c v ->
            { row = r
              col = c
              value = v |> string |> int }))
    |> Seq.collect id
    |> Seq.toList

let surrounding octo octos =
    [ for r in -1 .. 1 do
          for c in -1 .. 1 -> (r + octo.row, c + octo.col) ]
    |> Seq.map (fun (row, col) ->
        octos
        |> Seq.tryFind (fun o -> o.row = row && o.col = col))
    |> Seq.choose id

let increment octo = { octo with value = octo.value + 1 }

let reset octo = { octo with value = 0 }

let rec flash octos flashed =
    let flashing =
        octos |> List.tryFind (fun o -> o.value > 9)

    match flashing with
    | None -> (octos, flashed)
    | Some f ->
        let flashed = (f :: flashed)
        let neighbors = octos |> surrounding f

        let octos =
            octos
            |> List.filter (fun o -> o <> f)
            |> List.map (fun o ->
                if neighbors |> Seq.contains o then
                    increment o
                else
                    o)

        flash octos flashed

let rec step octos count iteration stopper =
    let octos = octos |> List.map increment
    let (notFlashed, flashed) = flash octos List.empty

    if stopper iteration notFlashed then
        (count, iteration + 1)
    else
        let flashed = flashed |> List.map reset
        let count = count + (flashed |> List.length)
        let octos = List.append flashed notFlashed
        step octos count (iteration + 1) stopper

let (flashCount, _) =
    step octos 0 0 (fun iteration _ -> iteration = 100)

printfn "Part 1 %i" flashCount

let (_, iteration) =
    step octos 0 0 (fun _ notFlashed -> notFlashed |> Seq.isEmpty)

printfn "Part 2 %i" iteration
