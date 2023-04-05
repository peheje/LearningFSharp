module Tests

open System
open Xunit

open App.Greeter
open App.SomeService

[<Fact>]
let ``Greet says goodmorning in the morning`` () =
    Assert.Equal ("Goodmorning", greet (fun () -> 5))

[<Fact>]
let ``Greet says hi when it's not morning`` () =
    Assert.Equal ("Hi!", greet (fun () -> 11))

[<Fact>]
let ``getPerson returns from cache if exists`` () =

    let cacheThatReturnsSome id = Some "cached value"

    Assert.Equal ("cached value", getPerson cacheThatReturnsSome 1)

[<Fact>]
let ``getPerson returns person not from cache if not there`` () =

    let cacheThatReturnsNone id = None

    Assert.Equal ("person not from cache", getPerson cacheThatReturnsNone 1)