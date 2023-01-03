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
let id id = document.getElementById id
let name id = document.getElementsByName id

let liked = id "liked" :?> HTMLTextAreaElement
let nameText = id "name" :?> HTMLTextAreaElement
let yes = id "yes" :?> HTMLButtonElement
let no = id "no" :?> HTMLButtonElement
let copy = id "copy" :?> HTMLButtonElement
let clear = id "clear" :?> HTMLButtonElement
let girl = id "girl" :?> HTMLInputElement
let boy = id "boy" :?> HTMLInputElement

let appendLiked message =
    liked.textContent <- message + "\n" + liked.textContent

let capitalizeName (name: string) =
    let nameSeparator = if name.Contains('-') then '-' else ' '

    name
    |> split nameSeparator
    |> Array.map (fun part -> part.Substring(0, 1).ToUpper() + part.Substring(1).ToLower())
    |> join nameSeparator

let nameIterator names =
    let liked = getLocalStorageOrEmpty "liked" |> split ';'
    let disliked = getLocalStorageOrEmpty "disliked" |> split ';'
    liked |> Array.rev |> join '\n' |> appendLiked
    let freeNames = names |> Array.map capitalizeName |> Array.except liked |> Array.except disliked
    let mutable index = -1

    let currentName () =
        freeNames[index]

    let nextName () =
        index <- index + 1
        currentName ()

    let askNext () =
        nameText.textContent <- sprintf "Do you like %s?" (nextName ())

    (nextName, currentName, askNext)

let boyOrGirlNames () =
    let savedGender = getLocalStorageOrEmpty "gender"
    match savedGender with
    | "boy" -> boyNames
    | _ -> girlNames

let initializeGenderChange () = 
    let savedGender = getLocalStorageOrEmpty "gender"
    match savedGender with
    | "boy" -> boy.checked <- true
    | _ -> girl.checked <- true

    match document.querySelector("""input[name="gender"]:checked""") with
    | :? HTMLInputElement as el ->
        setLocalStorage "gender" el.value
        document.location.reload()
    | _ -> printfn "Could not find checked"

    boy.onchange <- fun _ ->
        
    girl.onchange <- fun _ ->
        setLocalStorage "gender" "girl"
        document.location.reload()

    

initializeGenderChange()

let (nextName, currentName, askNext) = nameIterator (boyOrGirlNames())

let appendToLocalStorage key name =
    let current = getLocalStorageOrEmpty key

    if current = "" then
        setLocalStorage key name
    else
        setLocalStorage key (current + ";" + name)

[<Emit("navigator.clipboard.writeText($0)")>]
let writeToClipboard _text : JS.Promise<unit> = jsNative

let copyLikedToClipboard _ =
    async {
        try
            do! liked.textContent |> writeToClipboard |> Async.AwaitPromise
        with ex ->
            printfn "Promise rejected %s" ex.Message
    }
    |> Async.StartImmediate

let likeCurrentName _ =
    currentName () |> appendToLocalStorage "liked"
    currentName () |> appendLiked

let dislikeCurrentName _ =
    currentName () |> appendToLocalStorage "disliked"

let confirmClear _ =
    let prompt = "delete all liked and disliked names"
    if window.prompt $"Type '{prompt}' to continue." = prompt then 
        setLocalStorage "liked" ""
        setLocalStorage "disliked" ""
        window.location.reload ()

askNext ()

copy.onclick <- copyLikedToClipboard
yes.onclick <- likeCurrentName >> askNext
no.onclick <- dislikeCurrentName >> askNext
clear.onclick <- confirmClear
