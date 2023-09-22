module Learning.DifferentTypeParallelTask

open System.IO
open System.Threading.Tasks

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

let run () =
    printfn "started"

    let mainTask = task {
        let fileAsync = readTheFile ()
        let answerAsync = doSomeWork ()

        let! file = fileAsync
        let! answer = answerAsync

        printfn "%s" file
        printfn "%i" answer
        
    }

    mainTask.Wait ()

    printfn "ended"