module Learning.GetWebpages

open System

let private dump data =
    async {
        let path = $"dump/{Guid.NewGuid ()}.txt"
        do! IO.File.WriteAllTextAsync (path, data) |> Async.AwaitTask
    }

let private dumpSync data =
    let path = $"dump/{Guid.NewGuid ()}.txt"
    IO.File.WriteAllText (path, data)

let private getUrl url =
    async {
        try
            let! response = FSharp.Data.Http.AsyncRequestString (url, timeout = 5000)
            return Some response
        with
        | _ ->
            printfn $"Failed to download {url}"
            return None
    }

let private readUrlsList () =
    async {
        let path = "urls.txt"
        return! IO.File.ReadAllLinesAsync path |> Async.AwaitTask
    }

module Option =
    module Async =
        let bindAsync f x =
            async {
                match! x with
                | None -> return None
                | Some v ->
                    let! result = f v
                    return Some result
            }

        let bind f x =
            async {
                match! x with
                | None -> return None
                | Some x -> return Some (f x)
            }

let run () =
    printfn "running GetWebpages"

    async {
        let! urls = readUrlsList ()

        do!
            urls
            |> Seq.map getUrl
            |> Seq.map (Option.Async.bindAsync dump)
            // |> Seq.map (Option.Async.bind dumpSync)
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously