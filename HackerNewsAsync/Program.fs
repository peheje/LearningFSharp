open System
open System.Diagnostics
open System.Collections.Concurrent
open HackerNewsAsync.Model
open HackerNewsAsync
open System.Collections.Generic

let sw = Stopwatch.StartNew()

let queue = new BlockingCollection<int>(1)

let producer =
    async {
        let! ids = HnClient.getTopStoriesIds 1000

        for id in ids do
            queue.Add(id)

        queue.CompleteAdding()
    }

let results = ConcurrentDictionary<int, Story>()

let consumer =
    async {
        try
            while true do
                let! story =
                    queue.Take(Threading.CancellationToken.None)    // Todo, should find a non-blocking version of this
                    |> HnClient.getStory

                results.TryAdd(story.id, story) |> Debug.Assert
                printfn "Thread %i Received %s" Threading.Thread.CurrentThread.ManagedThreadId story.title
        with
        | :? InvalidOperationException -> printfn "Consumer ended"
    }

async {
    // I need the producer to run in it's own thread, not shared by the threadpool, because I rely on the consumer being allowed to block the thread with .Take
    let! p = Async.StartChild producer

    let consumers = List<Async<unit>>();

    for _ in 0..3 do
        let! c = Async.StartChild consumer
        consumers.Add(c)

    do! p

    for i in 0..3 do
        do! consumers[i]

}
|> Async.RunSynchronously

results.Values
|> Seq.filter (fun s -> s.typ = "story")
|> Seq.sortBy (fun s -> s.id)
|> Seq.length
|> printfn "%A"
