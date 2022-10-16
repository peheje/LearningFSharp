open System
open System.Diagnostics
open System.Net.Http
open System.Net.Http.Json
open System.Text.Json.Serialization
open System.Collections.Concurrent

type Story =
    { id: int
      title: string
      [<JsonPropertyName("type")>]
      typ: string
      url: string }

let client =
    new HttpClient(new SocketsHttpHandler(PooledConnectionLifetime = TimeSpan.FromMinutes(2)))

let getStory (id: int) =
    client.GetFromJsonAsync<Story>($"https://hacker-news.firebaseio.com/v0/item/{id}.json").Result

let sw = Stopwatch.StartNew()

let topStoriesPath = "https://hacker-news.firebaseio.com/v0/topstories.json"
let topStories = client.GetFromJsonAsync<int array>(topStoriesPath).Result |> Seq.take 20
let queue = new BlockingCollection<int>()
let results = ConcurrentDictionary<int, Story>()
let producer = async {
    for id in topStories do
        queue.Add(id)
        printfn "producer added %i" id
    queue.CompleteAdding()
}

let consumer = async {
    try
        while true do
            let story = queue.Take(Threading.CancellationToken.None) |> getStory
            results.TryAdd(story.id, story) |> Debug.Assert
            printfn "Thread %i Received %s" Threading.Thread.CurrentThread.ManagedThreadId story.title
    with
        | :? InvalidOperationException -> printfn "Consumer ended"
}

let maxConcurrent = 8
let consumers = [for _ in 0..maxConcurrent -> consumer]

(producer :: consumers)
|> Async.Parallel
|> Async.RunSynchronously
|> ignore

results.Values
    |> Seq.filter (fun s -> s.typ = "story")
    |> Seq.sortBy (fun s -> s.id)
    |> printfn "%A"

(*

let throttle = new Threading.SemaphoreSlim(10)

let storiesJson =
    topStories
    |> Array.take 40
    |> Array.Parallel.collect (fun id ->
        try
            throttle.Wait()
            [| (getStory id) |]
        finally
            throttle.Release() |> ignore)
    |> Array.filter (fun story -> story.typ = "story")
    |> Array.take 30
    |> Array.sortBy (fun story -> story.id)
    |> JsonSerializer.Serialize

sw.Stop()

printfn "Took %ims" sw.ElapsedMilliseconds

System.IO.File.WriteAllText("/Users/phj/Code/F-Sharp-Advent-of-Code-2021/HackerNewsAsync/data.json", storiesJson)
*)
