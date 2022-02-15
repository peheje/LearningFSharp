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

let surrounding octos octo =
    [ for r in -1..1 do for c in -1..1 -> (r + octo.row, c + octo.col) ]
    |> Seq.map (fun (row, col) -> octos |> Seq.tryFind (fun o -> o.row = row && o.col = col))
    |> Seq.choose id

let increment octo = { octo with value = octo.value + 1 }

let reset octo = { octo with value = 0 }

let rec flash octos flashed =
    let flashing = octos |> List.tryFind (fun o -> o.value > 9)

    match flashing with
    | None -> (octos, flashed)
    | Some f ->
        let flashed = (f :: flashed)

        let neighbors = surrounding octos f
        let octos =
            octos
            |> List.filter (fun o -> o <> f)
            |> List.map (fun o ->
                if neighbors |> Seq.contains o then
                    increment o
                else
                    o)

        flash octos flashed

let rec step octos count iteration =
    let octos = octos |> List.map increment
    let (octos, flashed) = flash octos List.empty

    if iteration = 100 then
        count
    else
        let flashed = flashed |> List.map reset
        let count = count + (flashed |> List.length)
        let octos = List.append flashed octos
        step octos count (iteration + 1)

let stepCount = step octos 0 0

printfn "%i" stepCount
