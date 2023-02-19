module Heartbeat

open System
open System.Collections.Generic
open Browser
open Html
open Browser.Types

let initHeartbeat () =
    let mutable pressed = false
    let beats = List<DateTimeOffset>()
    let beatElement = fromId "beat"
    let heartbeatElement = fromId "heartbeat"

    let showBeatIndicator () =
        beatElement.removeAttribute("hidden")
        window.setTimeout((fun _ ->
            beatElement.setAttribute("hidden", "hidden")
        ), 200) |> ignore

    let keepOnlyNewestNBeats n =
        if beats |> Seq.length = n then
            beats.RemoveAt 0

    let beatKeyNotPressed _ = pressed <- false

    let beatKeyPressed (event: Event) =
        let keyboardEvent = event :?> KeyboardEvent
        let key = keyboardEvent.key
        printfn "%A" key

        if pressed || key <> " " then ()
        else

        pressed <- true

        showBeatIndicator ()
        
        keepOnlyNewestNBeats 6

        beats.Add DateTimeOffset.UtcNow

        if beats |> Seq.length < 2 then ()
        else
        
        let averageTimeBetweenBeatMs =
            beats
            |> Seq.pairwise
            |> Seq.map (fun (first, second) ->
                let delta = second.ToUnixTimeMilliseconds () - first.ToUnixTimeMilliseconds ()
                delta |> float
            )
            |> Seq.average
        
        let bpm = 60000.0 / averageTimeBetweenBeatMs
        heartbeatElement.innerHTML <- bpm.ToString("#.##") + " bpm"

    document.addEventListener("keyup", beatKeyNotPressed)
    document.addEventListener("mouseup", beatKeyNotPressed)
    document.addEventListener("keydown", beatKeyPressed)