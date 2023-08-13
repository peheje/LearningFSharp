open System.Collections.Generic
open System.IO

let sw = System.Diagnostics.Stopwatch.StartNew()
let path = "input12.txt"
let rows = File.ReadAllLines path
type EdgeId = EdgeId of (int * int)

let readMap rows =
    let mutable destination = None
    let mutable source = None

    let map =
        rows
        |> Array.mapi (fun y row ->
            row
            |> Seq.toArray
            |> Array.mapi (fun x char ->
                let code =
                    match char with
                    | 'S' ->
                        source <- Some(EdgeId(x, y))
                        int 'a'
                    | 'E' ->
                        destination <- Some(EdgeId(x, y))
                        int 'z'
                    | _ -> int char

                code - 97))

    (map, destination.Value, source.Value)

let map, destination, source = readMap rows

let neighborIndices x y =
    let lastRowIndex = (map |> Array.length) - 1
    let lastColIndex = (map |> Array.head |> Array.length) - 1

    [| if x > 0 then yield EdgeId(x - 1, y)
       if y > 0 then yield EdgeId(x, y - 1)
       if x < lastColIndex then
           yield EdgeId(x + 1, y)
       if y < lastRowIndex then
           yield EdgeId(x, y + 1) |]

let infinite = 1000

let graph = Dictionary<EdgeId, array<(EdgeId * int)>>()

map
|> Array.iteri (fun y row ->
    row
    |> Array.iteri (fun x value ->
        let indices =
            (neighborIndices x y)
            |> Array.map (fun neighbor ->
                let (EdgeId (nx, ny)) = neighbor
                let difference = map[ny][nx] - value
                let cost = if difference > 1 then infinite else 1
                (neighbor, cost))

        graph[(EdgeId(x, y))] <- indices))

let dijkstra source =
    let distances = Dictionary<EdgeId, int>()
    let previous = Dictionary<EdgeId, option<EdgeId>>()
    let queue = PriorityQueue<EdgeId, int>()

    for v in graph.Keys do
        distances[v] <- infinite
        previous[v] <- None
        queue.Enqueue(v, infinite)

    distances[source] <- 0

    while queue.Count > 0 do
        let edge = queue.Dequeue()

        graph[edge]
        |> Array.iter (fun (neighbor, weight) ->
            let totalDistance = distances[edge] + weight

            if totalDistance < distances[neighbor] then
                distances[neighbor] <- totalDistance
                previous[neighbor] <- Some edge
                queue.Enqueue(neighbor, totalDistance))

    previous

let steps source =

    let previous = dijkstra source

    let mutable cursor = destination

    [| while cursor <> source do
           let step = previous[cursor]
           if step.IsSome then yield step.Value
           cursor <- step.Value |]
    |> Array.length

let sources = List<EdgeId>()

for i in 0 .. rows.Length - 1 do
    for j in 0 .. rows[i].Length - 1 do
        if rows[i][j] = 'a' then
            sources.Add(EdgeId(j, i))

let shortestPathFromLowestPoint = sources |> Seq.map steps |> Seq.min

printfn "Part1: Done with shortest path %i" (steps source)
printfn "Part2: shortest path from lowest point %A" shortestPathFromLowestPoint

sw.Stop()

printfn "Elapsed time %ims" sw.ElapsedMilliseconds
