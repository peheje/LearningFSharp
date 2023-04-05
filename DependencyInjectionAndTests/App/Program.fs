module App

module Greeter =
    let greet getHour =
        let hour = getHour ()
        if hour > 4 && hour < 10 then "Goodmorning" else "Hi!"

// Somewhere in production code, you don't want to have to say
// to use UtcNow everytime to get hour, so you create a helper
let greeterRealTime () =
    Greeter.greet (fun () -> System.DateTimeOffset.UtcNow.Hour)

greeterRealTime () |> printfn "%s"
greeterRealTime () |> printfn "%s"
greeterRealTime () |> printfn "%s"
