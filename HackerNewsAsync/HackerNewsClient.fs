module HackerNewsAsync.HnClient

open System
open System.Net.Http
open System.Net.Http.Json
open HackerNewsAsync.Model

let private http =
    new HttpClient(
        new SocketsHttpHandler(PooledConnectionLifetime = TimeSpan.FromMinutes(2)),
        BaseAddress = Uri("https://hacker-news.firebaseio.com")
    )

let getStory id =
    Async.AwaitTask(http.GetFromJsonAsync<Story>($"v0/item/{id}.json"))

let getTopStoriesIds () =
    Async.AwaitTask(http.GetFromJsonAsync<int array>("v0/topstories.json"))
