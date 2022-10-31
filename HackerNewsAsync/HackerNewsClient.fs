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
    http.GetFromJsonAsync<Story>($"v0/item/{id}.json") |> Async.AwaitTask

let getTopStoriesIds () =
    http.GetFromJsonAsync<int array>("v0/topstories.json") |> Async.AwaitTask
