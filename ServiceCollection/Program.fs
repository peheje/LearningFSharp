open Models.Customer
open Services

let serialize object =
    System.Text.Json.JsonSerializer.Serialize(object)

let customer = getService<ICustomer> ()

printfn "%A" (serialize customer)

printfn "%A" (customer.Legal())

printfn "%A" (getInstancesCount ())