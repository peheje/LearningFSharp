open System.Net.Http
open System
open System.IO
open System.Threading

let urls = File.ReadAllLines("urls_small.txt")

// Goal is to read all url HTML text, but with a max of 8 at same time.

let client =
    new HttpClient(new SocketsHttpHandler(PooledConnectionLifetime = TimeSpan.FromMinutes(2)))

let mutex = new Mutex()

let safePrint print =
    mutex.WaitOne() |> ignore
    print ()
    mutex.ReleaseMutex() |> ignore

let tokenExpiringInSeconds (seconds: int) =
    (new CancellationTokenSource(TimeSpan.FromSeconds(seconds)))
        .Token

let urlAsyncs =
    urls
    |> Array.map (fun url ->
        async {
            let threadId = Thread.CurrentThread.ManagedThreadId
            safePrint (fun _ -> printfn "Getting url %s on thread %i" url threadId)

            let html =
                client
                    .GetStringAsync(
                        url,
                        (tokenExpiringInSeconds 2)
                    )
                    .Result

            safePrint (fun _ -> printfn "Got url %s on thread %i" url threadId)
            return html
        })

let sw = Diagnostics.Stopwatch.StartNew()

let maxConcurrent = 8

(urlAsyncs, maxConcurrent)
|> Async.Parallel
|> Async.RunSynchronously
|> Array.iter (fun html -> File.WriteAllText(Guid.NewGuid().ToString() + ".txt", html))

printfn "took: %i ms" sw.ElapsedMilliseconds
