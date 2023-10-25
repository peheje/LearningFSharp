module App.Main

open Browser
open Feliz.ViewEngine

open Html
open BabyNames
open Compare
open Alcohol
open Heartbeat
open Unique
open Memory
open Days
open Equation

let urls =
    [
        ("/compare.html", "Compare", initCompare)
        ("/unique.html", "Unique", initUnique)
        ("/alcohol.html", "Alcohol", initAlcohol)
        ("/heartbeat.html", "Heartbeat", initHeartbeat)
        ("/memory.html", "Memory", initMemory)
        ("/babynames.html", "Babynames", initBabyNames)
        ("/days.html", "Days", initDays)
        ("/equation.html", "Equation", initEquation)
        ("https://twitter.com/peheje", "Contact", id)
    ]

let nav =
    urls
    |> List.mapi (fun i (url, name, _) ->
        let active = if window.location.pathname = url then "active" else ""
        let notLast = i < (urls |> List.length) - 1
        [
            Html.a
                [
                    prop.href url
                    prop.text name
                    prop.classes [ active ]
                ]
            if notLast then Html.span [ Html.text " | " ]
        ]
    )
    |> List.collect id
    |> Html.nav
    |> Render.htmlView

handleCompareRedirect ()

let site = urls |> List.tryFind (fun (url, _, _) -> url = window.location.pathname)
match site with
| Some (_, _, init) -> init ()
| None -> failwith "unknown site!"
(fromId "menu").innerHTML <- nav