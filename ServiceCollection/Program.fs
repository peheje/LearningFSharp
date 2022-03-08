open Microsoft.Extensions.DependencyInjection

let serialize object =
    System.Text.Json.JsonSerializer.Serialize(object)

type ICustomer =
    abstract Name : string
    abstract Age : int
    abstract Legal : unit -> bool

let createCustomer name age =
    { new ICustomer with
        member this.Name = name
        member this.Age = age
        member this.Legal () = this.Age > 18
    }

let services = ServiceCollection()

services.AddTransient<ICustomer>((fun provider -> createCustomer "Peter" 17)) |> ignore

let provider = services.BuildServiceProvider()

let customer = provider.GetService<ICustomer>()

printfn "%A" (serialize customer)

printfn "%A" (customer.Legal())