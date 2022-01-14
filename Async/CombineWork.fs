module Learning.CombineWork

let private work1 a b =
    async {
        printfn "work1"
        do! Async.Sleep 2000
        return a + b
    }

let private work2 a b =
    async {
        printfn "work2"
        do! Async.Sleep 2000
        return a * b
    }

let private work3 a =
    async {
        printfn "work3"
        let! res = a
        do! Async.Sleep 2000
        return res * 2
    }

let private muchWork =
    [ work1 10 30; work2 10 30 ] |> Seq.map work3 |> Async.Parallel

let run () =
    printfn "running CombineWork"

    async {
        let! result = muchWork
        printfn "%A" result
    }
    |> Async.RunSynchronously