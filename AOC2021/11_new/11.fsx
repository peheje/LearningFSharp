// Let me try to rephrase the problem in a more functional manner, as the current
// explanation is very imperative.

// Step:
// Let all octos be in group octos
// Increase octos values by 1
// A: If one exceeds 9 move it to the group flashed
// increase its neighbors values by 1, repeat A until no more exceeds 9
// Move all flashed to group octos, reset moved, count how many moved

let logs x = printfn "%A" x

let lines = System.IO.File.ReadAllLines("data.txt")

type Octo = { row: int; col: int; value: int }

let parseInt a = a |> string |> int

let octos =
    lines
    |> Seq.mapi (fun i row ->
        row
        |> Seq.mapi (fun j v -> { row = i; col = j; value = parseInt v }))
    |> Seq.collect id
    |> Seq.toList

let isNeighborTo a b =
    let neighbors =
        [| (-1, -1)
           (-1, 0)
           (-1, 1)
           (0, -1)
           (0, 1)
           (1, -1)
           (1, 0)
           (1, 1) |]

    neighbors
    |> Array.exists (fun (r, c) ->
        let row = a.row + r
        let col = a.col + c
        b.row = row && b.col = col)

let rec flash octos flashed =
    let found = octos |> List.tryFind (fun o -> o.value > 9)

    match found with
    | None -> (octos, flashed)
    | Some f ->
        let flashed = (f :: flashed)

        let octos =
            octos
            |> List.filter (fun o -> o <> f)
            |> List.map (fun o ->
                if o |> isNeighborTo f then
                    { o with value = o.value + 1 }
                else
                    o)

        flash octos flashed

let rec step octos count iteration =
    let octos =
        octos
        |> List.map (fun o -> { o with value = o.value + 1 })

    let (octos, flashed) = flash octos List.empty

    if iteration = 100 then
        count
    else
        let flashed = flashed |> List.map (fun o -> { o with value = 0 })
        let flashCount = flashed |> List.length
        let octos = List.append flashed octos
        step octos (count + flashCount) (iteration + 1)

let ans = step octos 0 0

logs ans
