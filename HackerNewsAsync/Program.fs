open System
open System.Diagnostics
open System.Net.Http
open System.Net.Http.Json
open System.Text.Json
open System.Text.Json.Serialization

type Story =
    { id: int
      title: string
      [<JsonPropertyName("type")>]
      typ: string
      url: string }

let client =
    new HttpClient(new SocketsHttpHandler(PooledConnectionLifetime = TimeSpan.FromMinutes(2)))

let getStory (id: int) =
    client.GetFromJsonAsync<Story>($"https://hacker-news.firebaseio.com/v0/item/{id}.json").Result

    // Ideally the API should be able to query only stories, i.e. this should be handled by the backend database
    // Instead take 40 will almost always end up in at least 30 stories (type=story) after filtering for it
    // Could do it more efficient/clever client-side, but you would almost be re-implementing "select top 30 from x where type = story order by id"
    // And that problem is already most efficiently solved by database

let sw = Stopwatch.StartNew()

let topStories = client.GetStringAsync("https://hacker-news.firebaseio.com/v0/topstories.json").Result
let throttle = new Threading.SemaphoreSlim(10)

let storiesJson =
    topStories
    |> JsonSerializer.Deserialize<int array>
    |> Array.take 40
    |> Array.Parallel.collect (fun id ->
        try
            throttle.Wait()
            [| (getStory id) |]
        finally
            throttle.Release() |> ignore)
    |> Array.filter (fun story -> story.typ = "story")
    |> Array.take 30
    |> Array.sortBy (fun story -> story.id)
    |> JsonSerializer.Serialize

sw.Stop()

printfn "Took %ims" sw.ElapsedMilliseconds

System.IO.File.WriteAllText("/Users/phj/Code/F-Sharp-Advent-of-Code-2021/HackerNewsAsync/data.json", storiesJson)
