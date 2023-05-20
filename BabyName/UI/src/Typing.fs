module Typing

open Html
open Browser
open Fable.Core.JsInterop
open Fable.Core.JS
open System
open Browser.Types

let private input = inputFromId "input"
let private timer = fromId "timer"
let private target = "the quick brown fox jumps over the lazy dog"

let mutable startTime: DateTime option = None
let mutable timerId = -1

let private printElapsedTime () =
    let duration = (DateTime.UtcNow - startTime.Value)
    timer.innerText <- duration.TotalSeconds |> string

let private onKey (e: KeyboardEvent) =
    if e.key = "F5" then ()
    else
    if startTime |> Option.isNone then
        startTime <- Some DateTime.UtcNow
        timerId <- setInterval printElapsedTime 100

    if target.StartsWith input.value then
        document.body?style?backgroundColor <- "green"
        if target = input.value then
            document.body?style?backgroundColor <- "gold"
            clearInterval timerId
            printElapsedTime ()
    else
        document.body?style?backgroundColor <- "red"
    
let initTyping () =
    input.focus()
    input |> onKeyUp onKey
