let paths =
    [ "start-A"
      "start-b"
      "A-c"
      "A-b"
      "b-d"
      "A-end"
      "b-end" ]

type Node =
    { Name: string
      Destinations: seq<string>
      mutable Visited: bool }

let split s = (s: string).Split("-")

let nodes =
    paths
    |> Seq.map split
    |> Seq.collect id
    |> Seq.distinct
    |> Seq.map (fun cave ->
        let destinations =
            paths
            |> Seq.filter (fun path -> path.StartsWith(cave))
            |> Seq.map (fun path -> (split path)[1])

        { Name = cave
          Destinations = destinations
          Visited = false })
    |> Seq.toArray

let neighbors node =
    node.Destinations
    |> Seq.map (fun name -> nodes |> Array.find (fun node -> node.Name = name))

for node in nodes do
    printfn
        "node with name %s has these destinations %A"
        node.Name
        (node
         |> neighbors
         |> Seq.map (fun node -> node.Name))

let q = System.Collections.Generic.Queue<Node>()
let root = nodes |> Seq.item 0
root.Visited <- true
q.Enqueue root
while q.Count <> 0 do
    let v = q.Dequeue()
    printfn "%s" v.Name
    for w in neighbors v do
        if not w.Visited then
            w.Visited <- true
            q.Enqueue w
        else
            printfn "Was here"