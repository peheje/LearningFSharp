module App.Main

open Browser
open Feliz.ViewEngine

open BabyNames
open Compare
open Alcohol
open Heartbeat
open Unique
open Memory

let nav =
    let urls =
        [
            ("/compare.html", "Compare", initCompare)
            ("/unique.html", "Unique", initUnique)
            ("/alcohol.html", "Alcohol", initAlcohol)
            ("/heartbeat.html", "Heartbeat", initHeartbeat)
            ("/memory.html", "Memory", initMemory)
            ("/babynames.html", "Babynames", initBabyNames)
        ]
    let items =
        urls
        |> List.mapi (fun i (url, name, _) ->
            let active = if window.location.pathname = url then "active" else ""
            let notLast = i = (urls |> List.length) - 1 |> not
            [
                Html.a
                    [
                        prop.href url
                        prop.text name
                        prop.classes [ active ]
                    ]
                if notLast then Html.span [ Html.text " | " ]
            ]
        ) |> List.collect id

    Html.nav items |> Render.htmlView

match window.location.pathname with
| "/babynames.html" -> initBabyNames ()
| "/compare.html" -> initCompare ()
| "/alcohol.html" -> initAlcohol ()
| "/heartbeat.html" -> initHeartbeat ()
| "/unique.html" -> initUnique ()
| "/memory.html" -> initMemory ()
| "/compare/compare.html" ->
    window.setTimeout (fun _ ->
        window.location.pathname <- "/compare.html"
    , 3000) |> ignore

| _ -> failwith "unknown site!"

(document.getElementById "menu").innerHTML <- nav