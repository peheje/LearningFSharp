open CatModels

let cat = Cat("Gert")
printfn "%A" cat
cat.Name |> printfn "%A"
cat.Age |> printfn "%A"
cat.Age <- 2
cat.Age |> printfn "%A"
cat.Name <- "Hugo"
cat.Age |> printfn "%A"

open DogModels

let dog = makeDog "Hund"
printfn "%A" dog
let agedDog = dog |> setAge 2
printfn "%A" agedDog
let namedDog = agedDog |> setName "Fido"
printfn "%A" namedDog