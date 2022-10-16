namespace HackerNewsAsync.Model

open System.Text.Json.Serialization

type Story =
    { id: int
      title: string
      [<JsonPropertyName("type")>]
      typ: string
      url: string }