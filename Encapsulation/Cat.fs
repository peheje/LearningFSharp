module CatModels

type Cat(name) =

    // make constructor argument mutable
    let mutable _name = name

    // private function
    let getSound () = "meow"

    // manual property with custom get and setter
    member this.Name
        with get () = _name
        and set value =
            (getSound ()) |> printfn "%s"
            this.Age <- this.Age + 1
            _name <- value

    // auto property with get and set
    member val Age = 1 with get, set