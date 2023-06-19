open System.Collections.Generic
open System.IO

let sw = System.Diagnostics.Stopwatch.StartNew()
let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/kotlin/aoc2022/src/main/kotlin/input12.txt"
let rows = File.ReadAllLines path
type EdgeId = EdgeId of (int * int)

let mutable destination: EdgeId option = None
let mutable source: EdgeId option = None

let map = rows |> Array.mapi (fun y row ->
    row |> Seq.toArray |> Array.mapi (fun x char ->
        let code =
            match char with
            | 'S' ->
                source <- Some (EdgeId (x, y))
                int 'a'
            | 'E' ->
                destination <- Some (EdgeId (x, y))
                int 'z'
            | _ -> int char
        code - 97
    )
)

let neighborIndices x y =
    let lastRowIndex = (map |> Array.length) - 1
    let lastColIndex = (map |> Array.head |> Array.length) - 1
    [|
        if x > 0 then yield EdgeId (x - 1, y)
        if y > 0 then yield EdgeId (x, y - 1)
        if x < lastColIndex then yield EdgeId (x + 1, y)
        if y < lastRowIndex then yield EdgeId (x, y + 1)
    |]

let infinite = 1000

let graph = Dictionary<EdgeId, array<(EdgeId * int)>>()
map |> Array.iteri (fun y row ->
    row |> Array.iteri (fun x value ->
        let indices = (neighborIndices x y) |> Array.map (fun neighbor ->
            let (EdgeId (nx, ny)) = neighbor
            let difference = map[ny][nx] - value
            let cost = if difference > 1 then infinite else 1
            (neighbor, cost)
        )
        graph[(EdgeId (x, y))] <- indices
    )
)

let distances = Dictionary<EdgeId, int>()
let previous = Dictionary<EdgeId, option<EdgeId>>()
let queue = PriorityQueue<EdgeId, int>()

for v in graph.Keys do
    distances[v] <- infinite
    previous[v] <- None
    queue.Enqueue(v, infinite)
distances[source.Value] <- 0

while queue.Count > 0 do
    let edge = queue.Dequeue()
    graph[edge] |> Array.iter(fun (neighbor, weight) ->
        let totalDistance = distances[edge] + weight
        if totalDistance < distances[neighbor] then
            distances[neighbor] <- totalDistance
            previous[neighbor] <- Some edge
            queue.Enqueue(neighbor, totalDistance)
    )

let mutable cursor = destination.Value
let shortestPath = [|
    while cursor <> source.Value do
        let step = previous[cursor]
        if step.IsSome then yield step.Value
        cursor <- step.Value
|]

printfn "Done with shortest path %i" (shortestPath |> Array.length)
sw.Stop()

printfn "Elapsed time %ims" sw.ElapsedMilliseconds