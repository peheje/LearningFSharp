module App

open Browser.Dom
open Browser.Types
open System
open Data
open Fable.Core

let getLocalStorageOrEmpty key =
    match Browser.WebStorage.localStorage.getItem key with
    | null -> ""
    | x -> x

let setLocalStorage key value =
    Browser.WebStorage.localStorage.setItem (key, value)

let split (separator: char) (source: string) = source.Split separator
let join (separator: char) (source: string array) = String.Join(separator, source)
let id id = document.getElementById (id)

let liked = id "liked" :?> HTMLTextAreaElement
let nameText = id "name" :?> HTMLTextAreaElement
let yes = id "yes" :?> HTMLButtonElement
let no = id "no" :?> HTMLButtonElement
let copy = id "copy" :?> HTMLButtonElement

let appendLiked message =
    liked.textContent <- message + "\n" + liked.textContent

let capitalizeName (name: string) =
    let nameSeparator = if name.Contains('-') then '-' else ' '

    name
    |> split nameSeparator
    |> Array.map (fun part -> part.Substring(0, 1).ToUpper() + part.Substring(1).ToLower())
    |> join nameSeparator

let nameIterator () =
    let liked = getLocalStorageOrEmpty "liked" |> split ';'
    liked |> Array.rev |> join '\n' |> appendLiked

    let disliked = getLocalStorageOrEmpty "disliked" |> split ';'

    let nonProcessedNames =
        names |> Array.map capitalizeName |> Array.except liked |> Array.except disliked

    let mutable index = -1

    let currentName () =
        if index = -1 then "" else nonProcessedNames[index]

    let nextName () =
        index <- index + 1
        currentName ()

    (nextName, currentName)

let (nextName, currentName) = nameIterator ()

let askNext () =
    nameText.textContent <- sprintf "Do you like %s?" (nextName ())

let appendToLocalStorage key name =
    let current = getLocalStorageOrEmpty key

    if current = "" then
        setLocalStorage key name
    else
        setLocalStorage key (current + ";" + name)

let addToDisliked name = appendToLocalStorage "disliked" name

let addToLiked name =
    appendToLocalStorage "liked" name
    appendLiked name

askNext ()

[<Emit("navigator.clipboard.writeText($0)")>]
let private writeToClipboard _text : JS.Promise<unit> = jsNative

copy.onclick <-
    fun _ ->
        async {
            try
                do! liked.textContent |> writeToClipboard |> Async.AwaitPromise
            with ex ->
                printfn "Promise rejected %s" ex.Message
        }
        |> Async.StartImmediate

yes.onclick <-
    fun _ ->
        let name = currentName ()
        addToLiked name
        askNext ()

no.onclick <-
    fun _ ->
        let name = currentName ()
        addToDisliked name
        askNext ()
