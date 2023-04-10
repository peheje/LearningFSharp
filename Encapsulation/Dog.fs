module DogModels

type Dog = { name: string; age: int }

let private getSound () = "woof"

let makeDog name = { name = name; age = 1 }

let setAge age dog = { dog with age = age }

let setName name dog =
    printfn "%s" (getSound ())
    { dog with
        name = name
        age = dog.age + 1 }
