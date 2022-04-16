module Tools

let serialize object =
    System.Text.Json.JsonSerializer.Serialize(object)

let log x = printfn "%A" x