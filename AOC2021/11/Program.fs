let logs x = printfn "%A" x; x
let dumps x =
    let options = System.Text.Json.JsonSerializerOptions(WriteIndented = true)
    let json = System.Text.Json.JsonSerializer.Serialize(x, options)
    System.IO.File.WriteAllText("dump.json", json)
    x
let rows = System.IO.File.ReadAllLines "data.txt"
let split s = [|for c in s -> int (string c)|]
let cells = rows |> Array.map split
let lastIndex = Array.length cells - 1

type Coordinate = { row: int; col: int }

let validNeighborIndexes row col =
    let minRow = max 0 (row - 1)
    let maxRow = min lastIndex (row + 1)
    let minCol = max 0 (col - 1)
    let maxCol = min lastIndex (col + 1)
    let rowIndexes = [|for ri in minRow..maxRow -> ri|]
    let colIndexes = [|for ci in minCol..maxCol -> ci|]
    Array.allPairs rowIndexes colIndexes
    |> Array.filter ((<>) (row, col))
    |> Array.map (fun (ri, ci) -> { row = ri; col = ci })

type Octopus = { value: int; flashes: int; flashed: bool; coordinate: Coordinate; surrounding: Coordinate array }

let toOctopus value ri ci =
    {value = value
     flashes = 0
     flashed = false
     coordinate = { row = ri; col = ci }
     surrounding = validNeighborIndexes ri ci}

let increment octopus =
    { octopus with value = octopus.value + 1 }

let flash octopus =
    { octopus with value = 0; flashes = octopus.flashes + 1; flashed = true }

let getAffectedByFlash octi =
     octi
    |> Array.filter (fun octopus -> octopus.value > 9 && not octopus.flashed)
    |> Array.map (fun x -> x.surrounding)
    |> Array.collect id

let rec step octi = seq {
    yield octi

    // Increase energy level by 1 for all
    let mutable next = octi |> Array.map increment

    let mutable affectedByFlash = next |> getAffectedByFlash

    while not (affectedByFlash |> Seq.isEmpty) do
        for affected in affectedByFlash do
            let affectedOctopus, rest =
                next |> Array.partition (fun octopus -> octopus.coordinate = affected && not octopus.flashed)

            let incremented = affectedOctopus |> Array.map flash
            next <- Array.concat [|incremented; rest|]

    affectedByFlash <- next |> getAffectedByFlash

    let didFlash, rest =
        next |> Array.partition (fun octopus -> octopus.flashed)

    let reset = didFlash |> Array.map flash
    next <- Array.concat [|reset; rest|]

    yield! next |> step
}

let octi =
    cells
    |> Array.mapi (fun ri xs -> xs |> Array.mapi (fun ci v -> toOctopus v ri ci))
    |> Array.collect id
    |> step
    |> Seq.item 100
    |> Seq.map (fun x -> x.flashes)
    |> Seq.sum
    |> dumps
