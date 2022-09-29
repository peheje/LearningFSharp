open System
open System.Net.Http
open System.Text.Json

type Story =
    { id: int
      title: string
      [<Serialization.JsonPropertyName("type")>]
      typ: string
      url: string }

let client =
    new HttpClient(new SocketsHttpHandler(PooledConnectionLifetime = TimeSpan.FromMinutes(2)))

let parseTopIdsResponse (ids: string) = ids.Trim('[').Trim(']').Split(",")

let getStory (id: string) =
    task {
        let! response = client.GetAsync($"https://hacker-news.firebaseio.com/v0/item/{id}.json")
        let! json = response.Content.ReadAsStringAsync()
        let story = JsonSerializer.Deserialize<Story>(json)
        return story
    }

task {
    // Ideally the API should be able to query only stories, i.e. this should be handled by the backend database
    // Instead take 40 will almost always end up in at least 30 stories (type=story) after filtering for it
    // Could do it more efficient/clever client-side, but you would almost be re-implementing "select top 30 from x where type = story order by id"
    // And that problem is already most efficiently solved by database
    let! response = client.GetAsync("https://hacker-news.firebaseio.com/v0/topstories.json")
    let! responseString = response.Content.ReadAsStringAsync()

    let throttle = new Threading.SemaphoreSlim(8)
    let storiesJson =
        responseString
        |> parseTopIdsResponse
        |> Array.take 40
        |> Array.Parallel.collect (fun id ->
            try
                throttle.Wait()
                [| (getStory id).Result |]
            finally
                throttle.Release() |> ignore
        )
        |> Array.filter (fun story -> story.typ = "story")
        |> Array.take 30
        |> Array.sortBy (fun story -> story.id)
        |> JsonSerializer.Serialize

    System.IO.File.WriteAllText("/Users/phj/Code/F-Sharp-Advent-of-Code-2021/HackerNewsAsync/data.json", storiesJson)
}
|> Async.AwaitTask
|> Async.RunSynchronously
