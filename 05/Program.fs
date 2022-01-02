let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s
let split (c: string) (s: string) = s.Split c

type Coordinate = {x: int; y: int}
type Line = {start: Coordinate; stop: Coordinate}

let toCoordinate str =
    let ints = str |> split "," |> Array.map int
    {x = ints[0]; y = ints[1]}

let toLine s =
    let ss = s |> (split " -> ")
    {start = toCoordinate(ss[0]); stop = toCoordinate(ss[1])}

let isHorisontal line = line.start.y = line.stop.y

let isVertical line = line.start.x = line.stop.x

let isHorizontalOrVertical line = isHorisontal line || isVertical line

let trajectory line =
    let xstep = if line.start.x > line.stop.x then -1 else 1
    let ystep = if line.start.y > line.stop.y then -1 else 1
    if isHorisontal line then
        [| for x in line.start.x .. xstep .. line.stop.x -> {x = x; y = line.start.y} |]
    elif isVertical line then
        [| for y in line.start.y .. ystep .. line.stop.y -> {x = line.start.x; y = y} |]
    else
        let xs = [| for x in line.start.x .. xstep ..line.stop.x -> x |]
        let ys = [| for y in line.start.y .. ystep ..line.stop.y -> y |]
        Array.zip xs ys |> Array.map (fun v -> {x = fst v; y = snd v})

let lines =
    System.IO.File.ReadAllLines "5.txt"
    |> Array.map toLine

let solve lines =
    lines
    |> Array.map trajectory
    |> Array.collect id
    |> Array.countBy id
    |> Array.filter (fun v -> snd v >= 2)
    |> Array.length

let ans1 =
    lines
    |> Array.filter isHorizontalOrVertical
    |> solve
    |> logs

let ans2 =
    lines
    |> solve
    |> logs