module Html
open Browser.Types
open Browser
open System
open Fable.Core

let newline = '\n'
let split (separator: char) (source: string) = source.Split separator
let join (separator: char) (source: string array) = String.Join(separator, source)
let contains (target: string) (source: string) = source.Contains(target)

let getLocalStorageOrEmpty key =
    match localStorage.getItem key with
    | null -> ""
    | x -> x

let setLocalStorage key value =
    localStorage.setItem (key, value)

let appendToLocalStorageList key name =
    let current = getLocalStorageOrEmpty key

    if current = "" then
        setLocalStorage key name
    else
        setLocalStorage key (current + ";" + name)

let fromId id = document.getElementById id
let areaFromId id = (fromId id) :?> HTMLTextAreaElement
let inputFromId id = (fromId id) :?> HTMLInputElement

let onClick action (el: HTMLElement) =
    el.onclick <- (fun _ -> action ())

[<Emit("navigator.clipboard.writeText($0)")>]
let private writeToClipboard _text : JS.Promise<unit> = jsNative

let toClipboard text =
    async {
        try
            do! text |> writeToClipboard |> Async.AwaitPromise
        with ex ->
            printfn "Promise rejected %s" ex.Message
    }
    |> Async.StartImmediate