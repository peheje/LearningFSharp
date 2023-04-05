module App

// I don't want to make a SQLite or Postgres or any real db, it's not necessary, instead this acts as something you can't really mock out, somewhere you have to call the selecting/saving, could be a SQL-client nuget package code
module NastyDatabaseDifficultToMockOut =
    let select id =
        "some dummy data"
    let insert id data =
        // don't save, just return unit
        ()

module CacheRepository =
    let get id =
        NastyDatabaseDifficultToMockOut.select id

    let save id data =
        NastyDatabaseDifficultToMockOut.insert id data

module Greeter =
    let greet getHour =
        let hour = getHour ()
        if hour > 4 && hour < 10 then "Goodmorning" else "Hi!"

// Somewhere in production code, you don't want to have to say to use UtcNow everytime to get hour, so you create a helper
let greeterRealTime () =
    Greeter.greet (fun () -> System.DateTimeOffset.UtcNow.Hour)

greeterRealTime () |> printfn "%s"
greeterRealTime () |> printfn "%s"
greeterRealTime () |> printfn "%s"
