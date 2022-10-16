open System
open System.Diagnostics
open System.Collections.Concurrent
open HackerNewsAsync.Model
open HackerNewsAsync

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
