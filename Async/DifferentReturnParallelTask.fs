module Learning.DifferentTypeParallelTask

open System.IO
open System.Threading.Tasks
open System
open System.Net.Http

let private readTheFile () =
    task {
        let path = "C:\Users\peter\Repos\LearningFSharp\Async\urls.txt"
        do! Task.Delay 2000
        return! File.ReadAllTextAsync path
    }

let private doSomeWork () =
    task {
        do! Task.Delay 2000
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

    let mainTask = task {
        let fileTask = readTheFile ()
        let workTask = doSomeWork ()
        let webTask = readTheUrl ()

        let! file = fileTask
        let! answer = workTask
        let! content = webTask

        printfn "%s" file
        printfn "%i" answer
        printfn "%s" content
        
    }

    mainTask.Wait ()

    printfn "ended"