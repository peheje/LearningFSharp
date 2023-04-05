module Tests

open System
open Xunit

open App.Greeter

[<Fact>]
let ``Greet says goodmorning in the morning`` () =
    Assert.Equal ("Goodmorning", greet (fun () -> 5))

[<Fact>]
let ``Greet says hi when it's not morning`` () =
    Assert.Equal ("Hi!", greet (fun () -> 11))