module Memory

open Browser
open Html

let private show id =
    let element = fromId id
    element.classList.add "inline-block"
    element.classList.remove "display-none"

let private hide id =
    let element = fromId id
    element.classList.remove "inline-block"
    element.classList.add "display-none"

type private State = Peek | Guess | Stopped

let initMemory () =

    let random = System.Random()
    let submitBtn = inputFromId "submit"
    let ioInput = inputFromId "io"
    let intervalInput = inputFromId "interval"
    let mutable state = Stopped
    let mutable number = ""

    let peek () =
        state <- Peek
        show "io-input"
        hide "length-input"
        hide "interval-input"
        ioInput.readOnly <- true
        submitBtn.disabled <- true
        submitBtn.innerText <- "Peeking.."
        let length = (inputFromId "length").valueAsNumber |> int
        let randomNumbers = Array.init length (fun _ -> random.Next 10 |> string) |> join ""
        number <- randomNumbers
        ioInput.value <- number
    
    let stopPeek () =
        state <- Guess
        ioInput.value <- ""
        ioInput.focus ()
        ioInput.readOnly <- false
        submitBtn.disabled <- false
        submitBtn.innerText <- "Guess"

    submitBtn |> onClick (fun _ ->
        show "restart"
        if state = Guess then
            if ioInput.value = number then
                window.alert "Correct"
            else
                window.alert ("Not correct, number was: " + number)
            state <- Stopped
        if state = Stopped then
            peek ()
            window.setTimeout((fun _ ->
                stopPeek ()
            ), (intervalInput.valueAsNumber * 1000.0) |> int) |> ignore
    )

    (fromId "restart") |> onClick window.location.reload

    document.addEventListener("keypress", (fun e -> 
        let keyboardEvent = e :?> Types.KeyboardEvent
        if keyboardEvent.key = "Enter" && state = Guess && ioInput.value.Length <> 0 then
            submitBtn.click ()
    ))