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

let isNeighbors a b =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]
    |> Seq.exists (fun (r, c) -> b.row = a.row + r && b.col = a.col + c)

let increment octo = { octo with value = octo.value + 1 }

let reset octo = { octo with value = 0 }

let rec flash octos flashed =
    let toFlash = octos |> List.tryFind (fun o -> o.value > 9)

    match toFlash with
    | None -> (octos, flashed)
    | Some f ->
        let flashed = (f :: flashed)

        let octos =
            octos
            |> List.filter (fun o -> o <> f)
            |> List.map (fun o ->
                if isNeighbors f o then
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
