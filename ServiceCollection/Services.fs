module Services

open Microsoft.Extensions.DependencyInjection
open Models.Customer

let mutable private getCount = 0

let private services = ServiceCollection()

services.AddTransient<ICustomer>(fun provider -> createCustomer "Peter" 17) |> ignore

let private provider = services.BuildServiceProvider()

let getService<'T> () =
    getCount <- getCount + 1
    provider.GetService<'T>()

let getInstancesCount () = getCount