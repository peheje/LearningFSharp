module Heartbeat

open System
open System.Collections.Generic
open Browser
open Html
open Browser.Types

let initHeartbeat () =
    let mutable pressed = false
    let beats = List<int64>()
    let beatElement = fromId "beat"
    let heartbeatElement = fromId "heartbeat"
    let heartbeatBtn = inputFromId "heartbeat-btn"

    let showBeatIndicator () =
        beatElement.removeAttribute("hidden")
        window.setTimeout((fun _ ->
            beatElement.setAttribute("hidden", "hidden")
        ), 200) |> ignore

    let limitBeats n =
        if beats |> Seq.length = n then
            beats.RemoveAt 0

    let beatKeyNotPressed _ = pressed <- false

    let beat key =
        if pressed || key <> " " then ()
        else

        pressed <- true
        showBeatIndicator ()
        limitBeats 6
        beats.Add (DateTimeOffset.UtcNow.ToUnixTimeMilliseconds ())

        if beats |> Seq.length < 2 then ()
        else
        
        let averageTimeBetweenBeatMs =
            beats
            |> Seq.pairwise
            |> Seq.map (fun (a, b) -> b - a |> float)
            |> Seq.average
        
        let bpm = 60000.0 / averageTimeBetweenBeatMs
        heartbeatElement.innerHTML <- bpm.ToString("#") + " bpm"

    let beatKeyPressed (event: Event) =
        let keyboardEvent = event :?> KeyboardEvent
        beat keyboardEvent.key

    let test () =
        window.setInterval((fun _ ->
            beat " "
            pressed <- false
        ), 500) |> ignore

    document.addEventListener("keyup", beatKeyNotPressed)
    document.addEventListener("mouseup", beatKeyNotPressed)
    document.addEventListener("keydown", beatKeyPressed)
    heartbeatBtn |> onClick (fun _ -> beat " ")