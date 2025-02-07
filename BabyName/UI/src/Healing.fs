module Healing

open Browser
open Browser.Types
open Fable.Core.JsInterop
open Html

// Helper functions
let private toSeq (nodeList: NodeListOf<Element>) = 
    Seq.init nodeList.length (fun i -> nodeList.item(i))

let private querySelectorAll selector = 
    document.querySelectorAll(selector) |> toSeq

// UI state management
let mutable private isHealingInProgress = false
let private random = System.Random()

// UI Selection
let private highlightSelected (selectedId: string) =
    // Remove previous selections
    querySelectorAll "label" |> Seq.iter (fun el -> el.classList.remove("selected-label"))
    querySelectorAll "progress" |> Seq.iter (fun el -> el.classList.remove("selected-progress"))
    
    // Add new selections
    document.querySelector($"label[for='{selectedId}']").classList.add("selected-label")
    document.querySelector($"progress#{selectedId}").classList.add("selected-progress")

// Progress bar management
let private updateProgressBars (updateFn: HTMLProgressElement -> unit) =
    querySelectorAll "progress"
    |> Seq.iter (fun (el: Element) ->
        let progress = el :?> HTMLProgressElement
        updateFn progress)

let private reduceProgress() =
    updateProgressBars (fun progress ->
        let randomReduction = random.Next(2, 6)
        progress.value <- max 0 (progress.value - float randomReduction))

// Cast bar visualization
let private showCastBar (castTime: int) =
    let castBarContainer = fromId "heal-cast-container" :?> HTMLElement
    let castBar = fromId "heal-cast-bar" :?> HTMLProgressElement
    
    castBarContainer.classList.remove("hidden")
    castBar.value <- 0.0
    
    let interval = 50.0
    let steps = int (float castTime / interval)
    let mutable step = 0
    let mutable intervalId = -1.0
    
    let updateBar() =
        step <- step + 1
        castBar.value <- (float step / float steps) * 100.0
        if step >= steps then
            castBarContainer.classList.add("hidden")
            window.clearInterval(intervalId)
            
    intervalId <- window.setInterval(updateBar, int interval)

// Healing functions
let private performHeal (castTime: int) (healingFn: unit -> unit) =
    if not isHealingInProgress then
        isHealingInProgress <- true
        showCastBar castTime
        window.setTimeout((fun _ ->
            healingFn()
            isHealingInProgress <- false
        ), castTime) |> ignore

let private healSelected() =
    let healTarget() =
        let selectedProgress = document.querySelector("progress.selected-progress") :?> HTMLProgressElement
        if not (isNull selectedProgress) then
            selectedProgress.value <- min selectedProgress.max (selectedProgress.value + 40.0)
    
    performHeal 2000 healTarget

let private groupHeal() =
    let healGroup() =
        updateProgressBars (fun progress ->
            progress.value <- min progress.max (progress.value + 10.0))
    
    performHeal 3000 healGroup

// Key handling
let private onKeyPress (event: Event) =
    match (event :?> KeyboardEvent).key with
    | "1" | "2" | "3" | "4" | "5" as key -> highlightSelected $"player{key}"
    | "H" | "h" -> healSelected()
    | "G" | "g" -> groupHeal()
    | _ -> ()

// Module initialization
let initHealing() =
    document.addEventListener("keydown", onKeyPress)
    window.setInterval(reduceProgress, 1000) |> ignore

let aboutHealing = "Healing"