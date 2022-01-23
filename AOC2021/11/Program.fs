let logs x = printfn "%A" x; x
let rows = System.IO.File.ReadAllLines "data.txt"
let split s = [|for c in s -> int (string c)|]
let cells = rows |> Array.map split
let lastIndex = Array.length cells - 1

let map f xxs =
    xxs |> Array.map (fun xs -> xs |> Array.map f)

let print xxs =
    let printable = xxs |> map string |> Seq.map (fun xs -> xs |> String.concat "")
    System.IO.File.WriteAllLines("output.txt", printable)

type Octo = { mutable value: int; mutable flashed: bool }

let printOctie octie =
    octie |> map (fun o -> o.value) |> print

let maxMin x = [|max 0 (x - 1)..min lastIndex (x + 1)|]

let validNeighborIndexes row col =
    Array.allPairs (maxMin row) (maxMin col)
    |> Array.filter ((<>) (row, col))

let increase o =
    o.value <- o.value + 1
    o

let octies = cells |> map (fun v -> { value = v; flashed = false })

let rec keepFlashing (octie: Octo array array): Octo array array =
    for r = 0 to (octie |> Array.length) - 1 do  
        for c = 0 to (cells[0] |> Array.length) - 1 do
            let current = (octie[r][c])

            if current.value > 9 && not current.flashed then
                current.flashed <- true

                let neighbors = validNeighborIndexes r c
                for (nr, nc) in neighbors do
                    let neighbor = (octies[nr][nc])
                    neighbor.value <- neighbor.value + 1
                    keepFlashing octie |> ignore
    octie

let step = 100

let mutable flashes = 0

for i = 0 to step - 1 do
    octies |> map increase |> ignore
    octies |> keepFlashing |> ignore

    for row in octies do
        for o in row do
            if o.flashed then flashes <- flashes + 1 

    octies |> map (fun o ->
        if o.flashed then o.value <- 0
        o.flashed <- false
    ) |> ignore

printOctie octies
logs flashes