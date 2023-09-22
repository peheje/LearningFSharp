module Learning.DifferentTypeParallelResult

open System.IO

type Result =
    | FileContent of string
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

let run () =
    printfn "started"
    
    let tasks = [ readTheFile (); doSomeWork () ]
    
    let results = Async.RunSynchronously (Async.Parallel tasks)
    
    results |> Array.iter (function
        | FileContent file -> printfn "%s" file
        | WorkResult answer -> printfn "%i" answer)

    printfn "ended"
