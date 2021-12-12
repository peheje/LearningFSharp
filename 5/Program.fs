let logs x = printfn "%A" x; x
let json s = System.Text.Json.JsonSerializer.Serialize(s, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
let debugs s = System.IO.File.WriteAllText("debug.json", json s); s
let split (c: string) (s: string) = s.Split c

type Coordinate = {x: int; y: int}
type Line = {start: Coordinate; stop: Coordinate}

let toCoordinate str =
    let ints = str |> split "," |> Array.map int
    {x = ints[0]; y = ints[1]}

let lines =
    System.IO.File.ReadAllLines "sample.txt"
    |> Array.map (split " -> ")
    |> Array.map (fun pair -> pair |> Array.map toCoordinate)
    |> Array.map (fun pair -> {start = pair[0]; stop = pair[1]})
    |> debugs
