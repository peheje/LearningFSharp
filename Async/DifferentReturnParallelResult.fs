module Learning.DifferentTypeParallelResult

open System
open System.IO
open System.Net.Http

type Result =
    | FileContent of string
    | WebResult of string
    | WorkResult of int

let private readTheFile () =
    async {
        let path = "C:\Users\peter\Repos\LearningFSharp\Async\urls.txt"
        do! Async.Sleep 2000
        let! fileContent = File.ReadAllTextAsync path |> Async.AwaitTask
        return FileContent fileContent
    }

let private doSomeWork () =
    async {
        do! Async.Sleep 2000
        return WorkResult 42
    }

let private readTheUrl () =
    let client =
        new HttpClient (new SocketsHttpHandler (PooledConnectionLifetime = TimeSpan.FromMinutes (2)))

    async {
        let! result = client.GetAsync ("https://www.google.com/") |> Async.AwaitTask
        let! content = result.Content.ReadAsStringAsync () |> Async.AwaitTask
        return WebResult (content.Substring(0, 100))
    }

let run () =
    printfn "started"

    let tasks = [ readTheFile (); doSomeWork (); readTheUrl () ]

    let results = Async.RunSynchronously (Async.Parallel tasks)

    results
    |> Array.iter (function
        | FileContent file -> printfn "%s" file
        | WorkResult answer -> printfn "%i" answer
        | WebResult result -> printfn "%s" result)

    printfn "ended"