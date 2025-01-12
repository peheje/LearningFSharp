module Healing

open Browser
open Browser.Types
open Fable.Core.JsInterop

let private toSeq (nodeList: NodeListOf<'T>) : seq<'T> =
    Seq.init nodeList.length (fun i -> nodeList.item(i))

let private highlightSelected (selectedId: string) =
    let allLabels = document.querySelectorAll("label") |> toSeq
    let allProgressBars = document.querySelectorAll("progress") |> toSeq

    allLabels |> Seq.iter (fun el -> el.classList.remove("selected-label"))
    allProgressBars |> Seq.iter (fun el -> el.classList.remove("selected-progress"))

    let selectedLabel = document.querySelector($"label[for='{selectedId}']")
    let selectedProgress = document.querySelector($"progress#{selectedId}")

    selectedLabel.classList.add("selected-label")
    selectedProgress.classList.add("selected-progress")

let private random = System.Random()
let mutable isHealingInProgress = false

let private reduceProgress () =
    let allProgressBars = document.querySelectorAll("progress") |> toSeq
    allProgressBars
    |> Seq.iter (fun (el: Element) ->
        let progress = el :?> HTMLProgressElement
        let currentValue = progress.value
        let randomReduction = random.Next(2, 6)
        progress.value <- max 0 (currentValue - float randomReduction)
    )

let private showCastBar (castTime: int) =
    let castBarContainer = document.querySelector("#heal-cast-container") :?> HTMLElement
    let castBar = document.querySelector("#heal-cast-bar") :?> HTMLProgressElement

    castBarContainer.classList.remove("hidden")
    castBar.value <- 0.0

    let interval = 50.0
    let totalTime = float castTime
    let steps = int (totalTime / interval)

    let mutable step = 0
    let mutable intervalId = -1.0
    let updateBar () =
        step <- step + 1
        castBar.value <- (step |> float) / (steps |> float) * 100.0
        if step >= steps then
            castBarContainer.classList.add("hidden")
            window.clearInterval(intervalId)
    intervalId <- window.setInterval(updateBar, int interval)

let private healSelected () =
    if not isHealingInProgress then
        isHealingInProgress <- true
        showCastBar 2000
        let selectedProgress = document.querySelector("progress.selected-progress") :?> HTMLProgressElement
        if not (isNull selectedProgress) then
            window.setTimeout((fun _ ->
                let currentValue = selectedProgress.value
                let maxValue = selectedProgress.max
                selectedProgress.value <- min maxValue (currentValue + 40.0)
                isHealingInProgress <- false
            ), 2000) |> ignore
        else
            isHealingInProgress <- false

let private groupHeal () =
    if not isHealingInProgress then
        isHealingInProgress <- true
        showCastBar 3000
        let allProgressBars = document.querySelectorAll("progress") |> toSeq
        window.setTimeout((fun _ ->
            allProgressBars
            |> Seq.iter (fun (el: Element) ->
                let progress = el :?> HTMLProgressElement
                let currentValue = progress.value
                let maxValue = progress.max
                progress.value <- min maxValue (currentValue + 10.0)
            )
            isHealingInProgress <- false
        ), 3000) |> ignore

let private onKeyPress (event: Event) =
    let keyboardEvent = event :?> KeyboardEvent
    match keyboardEvent.key with
    | "1" -> highlightSelected "player1"
    | "2" -> highlightSelected "player2"
    | "3" -> highlightSelected "player3"
    | "4" -> highlightSelected "player4"
    | "5" -> highlightSelected "player5"
    | "H" | "h" -> healSelected ()
    | "G" | "g" -> groupHeal ()
    | _ -> ()

let initHealing () =
    document.addEventListener("keydown", onKeyPress)
    window.setInterval(reduceProgress, 1000) |> ignore // Reduce progress every second

let aboutHealing =
    "Healing"
