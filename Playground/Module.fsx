type Person (name: string, bornYear: int) =
    let bornYear = bornYear
    member _.Name = name
    member _.Age = bornYear - 10

let peter = Person("Peter", 91)

printfn "%A" peter.Age