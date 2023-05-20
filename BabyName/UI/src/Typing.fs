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
let private storageKey = "best-typing-time"

let mutable private startTime: DateTime option = None
let mutable private timerId = -1

let private printElapsedTime () =
    let duration = (DateTime.UtcNow - startTime.Value)
    timer.innerText <- duration.TotalSeconds |> string
    duration.TotalMilliseconds

let private onKey (e: KeyboardEvent) =
    if e.key = "F5" then
        ()
    else
        if startTime |> Option.isNone then
            startTime <- Some DateTime.UtcNow
            timerId <- setInterval (fun _ -> printElapsedTime () |> ignore) 100

        if target.StartsWith input.value then
            document.body?style?backgroundColor <- "green"

            if target = input.value then
                document.body?style?backgroundColor <- "gold"
                clearInterval timerId
                let elapsedMs = printElapsedTime ()

                match getLocalStorageOrEmpty storageKey with
                | "" -> setLocalStorage storageKey (string elapsedMs)
                | previousBestMs ->
                    if (previousBestMs |> float) > elapsedMs then
                        setLocalStorage storageKey (string elapsedMs)

        else
            document.body?style?backgroundColor <- "red"

let initTyping () =
    match getLocalStorageOrEmpty storageKey with
    | "" -> ()
    | ms ->
        let bestElement = (fromId "best")
        bestElement.textContent <- sprintf "Best so far is %sms" ms
                
    input.focus ()
    input |> onKeyUp onKey
