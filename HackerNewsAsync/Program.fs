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
    let! response = client.GetAsync("https://hacker-news.firebaseio.com/v0/topstories.json")
    let! responseString = response.Content.ReadAsStringAsync()

    let storiesJson =
        responseString
        |> parseTopIdsResponse
        |> Array.take 40
        |> Array.Parallel.collect (fun id -> [| (getStory id).Result |])
        |> Array.filter (fun story -> story.typ = "story")
        |> Array.take 30
        |> Array.sortBy (fun story -> story.id)
        |> JsonSerializer.Serialize

    System.IO.File.WriteAllText("/Users/phj/Code/F-Sharp-Advent-of-Code-2021/HackerNewsAsync/data.json", storiesJson)
}
|> Async.AwaitTask
|> Async.RunSynchronously
