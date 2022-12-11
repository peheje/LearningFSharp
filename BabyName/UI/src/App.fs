module App

open Browser.Dom
open System
open Data

let splitOrEmpty (split: char) (s: string option) =
    match s with
    | Some v -> v.Split(split)
    | None -> [||]

let getLocalStorageOrEmpty key =
    match Browser.WebStorage.localStorage.getItem(key) with
    | null -> ""
    | x -> x

let split (separator: char) (source: string) =
    source.Split separator

let debug = document.querySelector("#debug") :?> Browser.Types.HTMLTextAreaElement
let nameText = document.querySelector("#name") :?> Browser.Types.HTMLTextAreaElement
let yes = document.querySelector("#yes") :?> Browser.Types.HTMLButtonElement
let no = document.querySelector("#no") :?> Browser.Types.HTMLButtonElement

let nameIterator () =
    let liked = getLocalStorageOrEmpty "liked" |> split ';'
    let disliked = getLocalStorageOrEmpty "disliked" |> split ';'
    let nonProcessedNames = names |> Array.except liked |> Array.except disliked
    let mutable index = -1
    let next () =
        index <- index + 1
        nonProcessedNames[index]
    next

let nextName = nameIterator()

nameText.textContent <- sprintf "Do you like %s?" (nextName ())

yes.onclick <- fun _ ->
    nameText.textContent <- sprintf "Do you like %s?" (nextName ())


