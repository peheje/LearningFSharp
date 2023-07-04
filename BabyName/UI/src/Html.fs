module Html

open Browser.Types
open Browser
open System
open Fable.Core

let newline = "\n"
let split (separator: string) (source: string) = source.Split separator
let join (separator: string) (source: string array) = String.Join(separator, source)
let contains (target: string) (source: string) = source.Contains(target)

let getLocalStorageOrEmpty key =
    match localStorage.getItem key with
    | null -> ""
    | x -> x

let setLocalStorage key value = localStorage.setItem (key, value)

let appendToLocalStorageList key name =
    let current = getLocalStorageOrEmpty key

    if current = "" then
        setLocalStorage key name
    else
        setLocalStorage key (current + ";" + name)

let fromId id = document.getElementById id
let areaFromId id = (fromId id) :?> HTMLTextAreaElement
let inputFromId id = (fromId id) :?> HTMLInputElement

let onChangeDebouncer milliseconds action (el: HTMLElement) =
    let mutable debounceTimer = 0.0

    el.onchange <-
        fun _ ->
            window.clearInterval debounceTimer
            debounceTimer <- window.setTimeout (action, milliseconds)

let onChange action (el: HTMLElement) = el.onchange <- fun _ -> action ()

let onChangeElement action (el: HTMLElement) = el.onchange <- fun _ -> action el

let onClick action (el: HTMLElement) = el.onclick <- fun _ -> action ()

let onClickEvent action (el: HTMLElement) = el.onclick <- fun event -> action event

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

let setTextArea id countId xs =
    (areaFromId id).value <- xs |> join newline
    (fromId countId).textContent <- xs |> Array.length |> string
