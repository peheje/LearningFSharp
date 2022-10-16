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

            return ids |> Seq.truncate n
        }

let sw = Stopwatch.StartNew()

let queue = new BlockingCollection<int>(1)

let producer =
    async {
        let! ids = HnClient.getTopStoriesIds 1000

        for id in ids do
            printfn "producer adding %i to queue" id
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

async {
    // I need the producer to run in it's own thread, not shared by the threadpool, because I rely on the consumer being allowed to block the thread with .Take
    let! p = producer |> Async.StartChild

    // But then how do I construct the consumers with a loop with this with the let! syntax?
    // And also, how do I instead start the consumers on the thread-pool?
    // This guys seems to do it, but that involves using task https://stackoverflow.com/questions/43239247/f-async-parallel-plus-main-thread
    let! c1 = consumer |> Async.StartChild
    let! c2 = consumer |> Async.StartChild
    let! c3 = consumer |> Async.StartChild
    let! c4 = consumer |> Async.StartChild

    do! p
    do! c1
    do! c2
    do! c3
    do! c4
}
|> Async.RunSynchronously

results.Values
|> Seq.filter (fun s -> s.typ = "story")
|> Seq.sortBy (fun s -> s.id)
|> Seq.length
|> printfn "%A"
