module Learning.GetWebpages

open System

let private dump data =
    async {
        let sw = Diagnostics.Stopwatch.StartNew ()
        let path = $"dump/{Guid.NewGuid ()}.txt"
        do! IO.File.WriteAllTextAsync (path, data) |> Async.AwaitTask
        return int sw.ElapsedMilliseconds
    }

let private dumpSync data =
    let path = $"dump/{Guid.NewGuid ()}.txt"
    IO.File.WriteAllText (path, data)

let private getUrl url =
    async {
        try
            let! response = FSharp.Data.Http.AsyncRequestString (url, timeout = 10000)
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

let bind f x =
    match x with | None -> None | Some v -> f v

let printList xs =
    xs |> Seq.toArray |> Array.map (Option.Async.bind (printfn "%ims"))

let run () =
    printfn "running GetWebpages"

    async {
        let! urls = readUrlsList ()

        do!
            urls
            |> Seq.map getUrl
            |> Seq.map (Option.Async.bindAsync dump)
            // |> Seq.map (Option.Async.bind dumpSync)
            |> printList
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously