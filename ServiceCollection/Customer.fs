module Models.Customer

type ICustomer =
    abstract Name: string
    abstract Age: int
    abstract Legal: unit -> bool

let createCustomer name age =
    { new ICustomer with
        member this.Name = name
        member this.Age = age
        member this.Legal() = this.Age > 18 }