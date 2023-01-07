module App

open Browser.Dom
open Browser.Types
open Data
open Html
open Browser

let private initGenderSelector () =
    let girl = id "girl" :?> HTMLInputElement
    let boy = id "boy" :?> HTMLInputElement

    girl.onchange <- fun _ ->
        setLocalStorage "gender" "girl"
        window.location.reload ()
    boy.onchange <- fun _ ->
        setLocalStorage "gender" "boy"
        window.location.reload ()

    if getLocalStorageOrEmpty "gender" = "boy" then
        boy.checked <- true
        boyNames
    else
        girl.checked <- true
        girlNames

let private initBabyNames () =
    let liked = getLocalStorageOrEmpty "liked" |> split ';'
    let disliked = getLocalStorageOrEmpty "disliked" |> split ';'
    let names = initGenderSelector ()
    let nameElement = id "name"
    let likedElement = id "liked" :?> HTMLTextAreaElement

    let appendLiked message =
        likedElement.textContent <- message + "\n" + likedElement.textContent

    let unprocessedNames =
        (names |> Array.except liked |> Array.except disliked |> Array.toSeq).GetEnumerator()

    let like () =
        unprocessedNames.Current |> appendToLocalStorageList "liked"
        unprocessedNames.Current |> appendLiked

    let dislike () =
        unprocessedNames.Current |> appendToLocalStorageList "disliked"

    let askNext () =
        unprocessedNames.MoveNext () |> ignore
        nameElement.textContent <- sprintf "Do you like %s?" (unprocessedNames.Current)

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
