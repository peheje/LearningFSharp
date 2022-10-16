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

let getStory id =
    let url = $"https://hacker-news.firebaseio.com/v0/item/{id}.json"
    client.GetFromJsonAsync<Story>(url).Result

let sw = Stopwatch.StartNew()

let topStories =
    let topStoriesPath = "https://hacker-news.firebaseio.com/v0/topstories.json"
    client.GetFromJsonAsync<int array>(topStoriesPath).Result
        |> Seq.take 20

let queue = new BlockingCollection<int>(1)
let results = ConcurrentDictionary<int, Story>()

let producer =
    async {
        for id in topStories do
            queue.Add(id)

        queue.CompleteAdding()
        printfn "Producer finished"
    }

let consumer =
    async {
        try
            while true do
                let story = queue.Take(Threading.CancellationToken.None) |> getStory
                results.TryAdd(story.id, story) |> Debug.Assert
                printfn "Thread %i Received %s" Threading.Thread.CurrentThread.ManagedThreadId story.title
        with
        | :? InvalidOperationException -> printfn "Consumer ended"
    }

let maxConcurrent = 8
let consumers = [ for _ in 0..maxConcurrent -> consumer ]

(producer :: consumers)
|> Async.Parallel
|> Async.RunSynchronously
|> ignore

results.Values
|> Seq.filter (fun s -> s.typ = "story")
|> Seq.sortBy (fun s -> s.id)
|> Seq.length
|> printfn "%A"
