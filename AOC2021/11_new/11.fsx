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

let rec step octos flashed count iteration increment =

    let octos =
        if increment then
            octos
            |> List.map (fun o -> { o with value = o.value + 1 })
        else
            octos

    let found = octos |> List.tryFind (fun o -> o.value > 9)

    match found with
    | None ->
        if iteration = 99 then
            count
        else
            let flashed =
                flashed
                |> List.map (fun o -> { o with value = 0 })

            let octos = List.append octos flashed
            step octos List.empty count (iteration + 1) true
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

        step octos flashed (count + 1) iteration false

let ans = step octos List.empty 0 0 true

logs ans
