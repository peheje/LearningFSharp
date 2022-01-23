let logs x = printfn "%A" x; x
let rows = System.IO.File.ReadAllLines "sample.txt"
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

type Octo = { value: int; ri: int; ci: int }

let neighbors octie ri ci =
    let indexes = surrounding ri ci
    octie |> Seq.filter (fun o -> indexes |> Array.contains (o.ri, o.ci))

let rec flashingLoop xxs flashed =
    let toFlash, rest = xxs |> Array.partition (fun o -> o.value > 9 && not (flashed |> Array.contains (o.ri, o.ci)))

    if toFlash |> Array.isEmpty then
        xxs
    else
        let flashedIndexes = toFlash |> Array.map (fun o -> o.ri, o.ci)
        let incrementTheseIndexes = toFlash |> Array.map (fun o -> surrounding o.ri o.ci) |> Array.collect id

        let incrementedOctoes =
            rest
            |> Array.filter (fun o -> incrementTheseIndexes |> Array.contains (o.ri, o.ci))
            |> Array.map (fun o -> { o with value = o.value + 1 })

        let totalFlashed = (Array.concat [|flashed; flashedIndexes|])
        flashingLoop incrementedOctoes totalFlashed


let flashing xxs =
    let octieSeq = xxs |> mapi (fun v ri ci -> { value = v; ri = ri; ci = ci }) |> Array.collect id
    let octie = flashingLoop octieSeq Array.empty
    
    octie |> 


let reset xxs =
    xxs |> mapic (fun x _ _ -> x > 9) (fun x _ _-> 0)

let step = increment >> flashing >> reset

let rec next octie = seq {
    yield octie
    print octie
    yield! octie |> step |> next
}

octie |> next |> Seq.item 1 |> logs