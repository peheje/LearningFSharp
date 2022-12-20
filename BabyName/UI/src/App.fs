module App

open Browser.Dom
open System
open Data
open Fable.Core
open Fable.Core.JsInterop

let getLocalStorageOrEmpty key =
    match Browser.WebStorage.localStorage.getItem key with
    | null -> ""
    | x -> x

let setLocalStorage key value =
    Browser.WebStorage.localStorage.setItem(key, value)

let split (separator: char) (source: string) =
    source.Split separator

let debug = document.querySelector("#debug") :?> Browser.Types.HTMLTextAreaElement
let nameText = document.querySelector("#name") :?> Browser.Types.HTMLTextAreaElement
let yes = document.querySelector("#yes") :?> Browser.Types.HTMLButtonElement
let no = document.querySelector("#no") :?> Browser.Types.HTMLButtonElement
let username = document.querySelector("#username") :?> Browser.Types.HTMLInputElement

let usernameIsValid username =
    username |> Seq.length > 4

let appendDebug message =
    debug.textContent <- sprintf "\n%s" message + debug.textContent

let capitalizeName (name: string) =
    let nameSeparator = if name.Contains('-') then '-' else ' '
    let parts = name.Split(nameSeparator)
    let capitalizedParts = parts |> Array.map (fun part ->
        part.Substring(0, 1).ToUpper() + part.Substring(1).ToLower()
    )
    String.Join(nameSeparator, capitalizedParts)

let nameIterator () =
    let liked = getLocalStorageOrEmpty "liked" |> split ';'
    appendDebug (String.Join('\n', liked))
    let disliked = getLocalStorageOrEmpty "disliked" |> split ';'
    let nonProcessedNames =
        names
        |> Array.map capitalizeName
        |> Array.except liked
        |> Array.except disliked
    let mutable index = -1
    let currentName () =
        if index = -1 then "" else nonProcessedNames[index]
    let nextName () =
        index <- index + 1
        currentName ()
    (nextName, currentName)

let (nextName, currentName) = nameIterator()

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
    appendDebug name

username.oninput <- fun _ ->
    if username.value |> usernameIsValid then
        setLocalStorage "username" username.value
        yes.disabled <- false
        no.disabled <- false

username.value <- getLocalStorageOrEmpty "username"
username.dispatchEvent(Browser.Event.Event.Create("input")) |> ignore

askNext ()

yes.onclick <- fun _ ->
    let name = currentName()
    addToLiked name
    askNext ()

no.onclick <- fun _ ->
    let name = currentName()
    addToDisliked name
    askNext ()

let [<Global("LZString")>] lzstring: JS.Console = jsNative