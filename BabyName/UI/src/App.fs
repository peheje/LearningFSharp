module App

open Browser.Dom
open System
open Data

// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

let nameText = document.querySelector("#names") :?> Browser.Types.HTMLTextAreaElement

// Register our listener
myButton.onclick <- fun _ ->
    count <- count + 1
    myButton.innerText <- sprintf "You clicked: %i time(s)" count

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

let liked = getLocalStorageOrEmpty "liked" |> split ';'
let disliked = getLocalStorageOrEmpty "disliked" |> split ';'

let nonProcessedNames = names |> Array.except liked |> Array.except disliked

nameText.textContent <- String.Join("\n", nonProcessedNames)