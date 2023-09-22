module Learning.DifferentTypeParallel

open System
open System.IO
open System.Net.Http

let private readTheFile () =
    async {
        let path = "C:\Users\peter\Repos\LearningFSharp\Async\urls.txt"
        do! Async.Sleep 2000
        return! File.ReadAllTextAsync path |> Async.AwaitTask
    }

let private doSomeWork () =
    async {
        do! Async.Sleep 2000
        return 42
    }

let private readTheUrl () =
    let client =
        new HttpClient (new SocketsHttpHandler (PooledConnectionLifetime = TimeSpan.FromMinutes (2)))

    async {
        let! result = client.GetAsync ("https://www.google.com/") |> Async.AwaitTask
        let! content = result.Content.ReadAsStringAsync () |> Async.AwaitTask
        return content.Substring(0, 100)
    }

let run () =
    printfn "started"

    async {
        let! fileAsync = readTheFile () |> Async.StartChild
        let! answerAsync = doSomeWork () |> Async.StartChild
        let! webAsync = readTheUrl () |> Async.StartChild

        let! file = fileAsync
        let! answer = answerAsync
        let! content = webAsync

        printfn "%s" file
        printfn "%i" answer
        printfn "%s" content
        
    } |> Async.RunSynchronously

    printfn "ended"