open System.Diagnostics
open System.Collections.Concurrent
open HackerNewsAsync.Model
open HackerNewsAsync
open System.Threading
open System.Threading.Channels

let sw = Stopwatch.StartNew()

module IdChannel =
    let private channel = Channel.CreateBounded<int>(1)

    let receive () =
        task {
            try
                let! id = channel.Reader.ReadAsync()
                return Some id
            with
            | :? ChannelClosedException -> return None
        }
        |> Async.AwaitTask

    let send id =
        channel.Writer.WriteAsync(id).AsTask()
        |> Async.AwaitTask

    let close () = channel.Writer.Complete()

let producer =
    async {
        let! ids = HnClient.getTopStoriesIds ()

        for id in ids do
            do! IdChannel.send (id)

        IdChannel.close ()
    }

let stories = ConcurrentDictionary<int, Story>()

let consumer =
    async {
        let threadId = Thread.CurrentThread.ManagedThreadId
        let mutable read = true

        while read do
            match! IdChannel.receive () with
            | Some storyId ->
                let! story = storyId |> HnClient.getStory
                stories.TryAdd(story.id, story) |> Debug.Assert
                printfn "Thread %i Received %s" threadId story.title
            | None ->
                read <- false
                printfn "Thread %i done" threadId
    }

async {
    let! p = Async.StartChild producer

    let! consumers =
        List.init 8 (fun _ -> Async.StartChild consumer)
        |> Async.Parallel

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
