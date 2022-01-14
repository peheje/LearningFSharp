module Learning.GetWebpages

open System

let private dump data =
    async {
        let path = $"dump/{Guid.NewGuid ()}.txt"
        do! IO.File.WriteAllTextAsync (path, data) |> Async.AwaitTask
    }

let private limitString maxLength s =
    let lastIndex = (s |> String.length) - 1
    let limit = min lastIndex maxLength
    s.Substring (0, limit)

let private getUrl url =
    async {
        try
            let! response = FSharp.Data.Http.AsyncRequestString (url, timeout = 5000)
            return Some response
        with
        | ex ->
            let message = ex.Message |> limitString 50
            printfn $"Failed to download {url} exception message {message}"
            return None
    }

let private readUrlsList () =
    async {
        let path = "urls.txt"
        return! IO.File.ReadAllLinesAsync (path) |> Async.AwaitTask
    }

let private mapOptionalAsync f asyncItem =
    async {
        match! asyncItem with
        | Some x ->
            let! result = f x
            return Some result
        | None -> return None
    }

let run () =
    printfn "running GetWebpages"

    async {
        let! urls = readUrlsList ()

        do!
            urls
            |> Seq.map getUrl
            |> Seq.map (mapOptionalAsync dump)
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously