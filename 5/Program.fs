let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s
let split (c: string) (s: string) = s.Split c

type Coordinate = {x: int; y: int}
type Line = {start: Coordinate; stop: Coordinate}

let toCoordinate str =
    let ints = str |> split "," |> Array.map int
    {x = ints[0]; y = ints[1]}

let toLine (xs: string array) =
    {start = toCoordinate(xs[0]); stop = toCoordinate(xs[1])}

let isHorizontalOrVertical line =
    line.start.x = line.stop.x || line.start.y = line.stop.y

let trajectory line =
    let xstep = if line.start.x > line.stop.x then -1 else 1
    let ystep = if line.start.y > line.stop.y then -1 else 1
    [|
        for x in line.start.x .. xstep .. line.stop.x do
            for y in line.start.y .. ystep ..line.stop.y do
                { x = x; y = y }
    |]

let lines =
    System.IO.File.ReadAllLines "sample.txt"
    |> Array.map (split " -> ")
    |> Array.map toLine
    |> Array.filter isHorizontalOrVertical
    |> Array.map trajectory
    |> debugs
