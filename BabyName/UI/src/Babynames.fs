module BabyNames

open Data
open Html
open Browser

let private initGenderSelector () =
    let girl = inputFromId "girl"
    let boy = inputFromId "boy"

    [|girl; boy|] |> Array.iter (fun el -> el |> onClick (fun _ ->
        setLocalStorage "gender" el.value
        window.location.reload ()))

    if getLocalStorageOrEmpty "gender" = "boy" then
        boy.checked <- true
        boyNames
    else
        girl.checked <- true
        girlNames

let initBabyNames () =
    let liked = getLocalStorageOrEmpty "liked" |> split ";"
    let disliked = getLocalStorageOrEmpty "disliked" |> split ";"
    let nameElement = fromId "name"
    let likedElement = areaFromId "liked"
    let mutable index = -1

    let unprocessedNames =
        initGenderSelector () |> Array.except liked |> Array.except disliked

    let appendLiked message =
        likedElement.textContent <- message + "\n" + likedElement.textContent

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

    liked |> Array.rev |> join newline |> appendLiked
    fromId "yes" |> onClick (like >> askNext)
    fromId "no" |> onClick (dislike >> askNext)
    fromId "clear" |> onClick confirmClear
    fromId "copy" |> onClick (fun _ -> toClipboard likedElement.textContent)

    askNext()