module Learning.DifferentTypeParallel

open System.IO
open System.Threading.Tasks

let private readTheFile () =
    async {
        let path = "/Users/phj/Desktop/AS/data.txt"
        do! Async.Sleep 2000
        return! File.ReadAllTextAsync path |> Async.AwaitTask
    }

let private doSomeWork () =
    async {
        do! Async.Sleep 2000
        return 42
    }

let run () =
    printfn "started"

    async {
        let! fileAsync = readTheFile () |> Async.StartChild
        let! answerAsync = doSomeWork () |> Async.StartChild

        let! file = fileAsync
        let! answer = answerAsync

        printfn "%s" file
        printfn "%i" answer
        
    } |> Async.RunSynchronously

    printfn "ended"