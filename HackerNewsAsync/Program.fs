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

module HnClient =
    let private http =
        new HttpClient(
            new SocketsHttpHandler(PooledConnectionLifetime = TimeSpan.FromMinutes(2)),
            BaseAddress = Uri("https://hacker-news.firebaseio.com")
        )

    let getStory id =
        async {
            return!
                http.GetFromJsonAsync<Story>($"v0/item/{id}.json")
                |> Async.AwaitTask
        }

    let getTopStoriesIds n =
        async {
            let! ids =
                http.GetFromJsonAsync<int array>("v0/topstories.json")
                |> Async.AwaitTask

            return ids |> Seq.take n
        }

let sw = Stopwatch.StartNew()

let queue = new BlockingCollection<int>(1)

let producer =
    async {
        let! ids = HnClient.getTopStoriesIds 20

        for id in ids do
            queue.Add(id)

        queue.CompleteAdding()
        printfn "Producer finished"
    }

let results = ConcurrentDictionary<int, Story>()

let consumer =
    async {
        try
            while true do
                let! story =
                    queue.Take(Threading.CancellationToken.None)
                    |> HnClient.getStory

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
