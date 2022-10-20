open System
open System.Diagnostics
open System.Collections.Concurrent
open HackerNewsAsync.Model
open HackerNewsAsync
open System.Threading.Channels

let sw = Stopwatch.StartNew()
let channel = Channel.CreateBounded<int>(1)

let producer =
    async {
        let! ids = HnClient.getTopStoriesIds 1000

        for id in ids do
            do! channel.Writer.WriteAsync(id).AsTask() |> Async.AwaitTask // In future might use this: https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1021-value-task-interop.md

        channel.Writer.Complete()
    }

let stories = ConcurrentDictionary<int, Story>()

let consumer =
    async {
        let threadId = Threading.Thread.CurrentThread.ManagedThreadId
        let mutable read = true
        while read do
            let! ready = channel.Reader.WaitToReadAsync().AsTask() |> Async.AwaitTask
            if ready then
                match channel.Reader.TryRead() with
                | (true, storyId) ->
                    let! story = storyId |> HnClient.getStory
                    stories.TryAdd(story.id, story) |> Debug.Assert
                    printfn "Thread %i Received %s" threadId story.title
                | (false, _) -> read <- false

        printfn "Thread %i done" threadId
    }

async {
    let! p = Async.StartChild producer
    let! consumers = List.init 8 (fun _ -> Async.StartChild consumer) |> Async.Parallel

    do! p

    for c in consumers do
        do! c
}
|> Async.RunSynchronously

stories.Values
|> Seq.filter (fun s -> s.typ = "story")
|> Seq.sortBy (fun s -> s.id)
|> Seq.length
|> printfn "%A"
