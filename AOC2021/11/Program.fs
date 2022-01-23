let logs x = printfn "%A" x; x
let rows = System.IO.File.ReadAllLines "data.txt"
let split s = [|for c in s -> int (string c)|]
let octie = rows |> Array.map split
let lastIndex = Array.length octie - 1

let map f xxs =
    xxs |> Array.map (fun xs -> xs |> Array.map f)

let mapic c f xxs =
    xxs |> Array.map (fun xs -> xs |> Array.map (fun x -> if c x then f x else x))

let maxMin x = [|max 0 (x - 1)..min lastIndex (x + 1)|]

let surrounding row col =
    Array.allPairs (maxMin row) (maxMin col)
    |> Array.filter ((<>) (row, col))

let increment xxs = xxs |> map ((+) 1)

let flashedThisStep = -1
let mutable flashes = 0
let rec flashing xxs =
    for ri, xs in xxs |> Array.indexed do
        for ci, v in xs |> Array.indexed do
            if v > 9 then
                xxs[ri][ci] <- flashedThisStep
                flashes <- flashes + 1

                let neighbors = surrounding ri ci
                for nri, nci in neighbors do
                    let neighbor = xxs[nri][nci]
                    if neighbor <> flashedThisStep then
                        xxs[nri][nci] <- neighbor + 1

    if xxs |> Array.exists (fun xs -> xs |> Array.exists (fun x -> x > 9)) then
        flashing xxs
    else
        xxs

let reset xxs = xxs |> mapic (fun x -> x > 9 || x = flashedThisStep) (fun _ -> 0)

let step = increment >> flashing >> reset

let rec next octie = seq {
    yield octie
    yield! octie |> step |> next
}

let output = 
    octie
    |> next
    |> Seq.item 100

logs flashes