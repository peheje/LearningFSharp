module Shared

// To make it easier to support Fable (https://fable.io/)
// unsupported F# features will be put in here, so only this file has to change

let random = System.Random.Shared

type Stopwatch () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    
    member _.ElapsedMilliseconds() =
        sw.ElapsedMilliseconds