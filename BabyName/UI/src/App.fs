module App

open Browser.Types
open Data
open Html
open Browser

let private initGenderSelector () =
    let girl = id "girl" :?> HTMLInputElement
    let boy = id "boy" :?> HTMLInputElement

    [|girl; boy|] |> Array.iter (fun el -> el |> onClick (fun _ ->
        setLocalStorage "gender" el.value
        window.location.reload ()))

    if getLocalStorageOrEmpty "gender" = "boy" then
        boy.checked <- true
        boyNames
    else
        girl.checked <- true
        girlNames

let private initBabyNames () =
    let liked = getLocalStorageOrEmpty "liked" |> split ';'
    let disliked = getLocalStorageOrEmpty "disliked" |> split ';'
    let nameElement = id "name"
    let likedElement = id "liked" :?> HTMLTextAreaElement
    let mutable index = -1

    let appendLiked message =
        likedElement.textContent <- message + "\n" + likedElement.textContent

    let unprocessedNames =
        initGenderSelector () |> Array.except liked |> Array.except disliked

    let like () =
        unprocessedNames[index] |> appendToLocalStorageList "liked"
        unprocessedNames[index] |> appendLiked

    let dislike () =
        unprocessedNames[index] |> appendToLocalStorageList "disliked"

    let askNext () =
        index <- index + 1
        nameElement.textContent <- sprintf "Do you like %s?" (unprocessedNames[index])

    let confirmClear () =
        let prompt = "delete all liked and disliked names"
        if window.prompt $"Type '{prompt}' to continue." = prompt then
            localStorage.clear()
            window.location.reload ()

    liked |> Array.rev |> join '\n' |> appendLiked
    id "yes" |> onClick (like >> askNext)
    id "no" |> onClick (dislike >> askNext)
    id "clear" |> onClick confirmClear
    id "copy" |> onClick (fun _ -> toClipboard likedElement.textContent)

    askNext()

initBabyNames ()
