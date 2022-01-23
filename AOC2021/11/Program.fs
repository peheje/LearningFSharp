let logs x = printfn "%A" x; x
let rows = System.IO.File.ReadAllLines "data.txt"
let split s = [|for c in s -> int (string c)|]
let octie = rows |> Array.map split
let lastIndex = Array.length octie - 1

let map f xxs =
    xxs |> Array.map (fun xs -> xs |> Array.map f)

let mapi f xxs =
    xxs |> Array.mapi (fun ri xs -> xs |> Array.mapi (fun ci x -> f ri ci x))

let mapic c f xxs =
    xxs |> Array.mapi (fun ri xs -> xs |> Array.mapi (fun ci x -> if c x ri ci then f x ri ci else x))

let print xxs =
    let printable = xxs |> map string |> Seq.map (fun xs -> xs |> String.concat "")
    System.IO.File.WriteAllLines("output.txt", printable)

let maxMin x = [|max 0 (x - 1)..min lastIndex (x + 1)|]

let surrounding row col =
    Array.allPairs (maxMin row) (maxMin col)
    |> Array.filter ((<>) (row, col))

let increment xxs =
    xxs |> map (fun x -> x + 1)

let mutable flashes = 0

let rec flashing xxs =
    for ri, xs in xxs |> Array.indexed do
        for ci, v in xs |> Array.indexed do
            if v > 9 then
                let neighbors = surrounding ri ci
                for nri, cri in neighbors do
                    let neighbor = xxs[nri][cri]
                    if neighbor >= 0 then
                        xxs[nri][cri] <- neighbor + 1
                xxs[ri][ci] <- -1
                flashes <- flashes + 1
    
    if xxs |> Array.exists (fun xs -> xs |> Array.exists (fun x -> x > 9)) then
        flashing xxs
    else
        xxs

let reset xxs =
    xxs |> mapic (fun x _ _ -> x > 9 || x < 0) (fun x _ _-> 0)

let step = increment >> flashing >> reset

let rec next octie = seq {
    yield octie
    print octie
    yield! octie |> step |> next
}

let output = 
    octie
    |> next
    |> Seq.item 100

logs flashes