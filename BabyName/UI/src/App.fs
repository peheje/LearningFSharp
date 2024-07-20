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
open Kahut

let urls =
    [
        ("/compare.html", "Compare", initCompare, aboutCompare)
        ("/unique.html", "Unique", initUnique, aboutUnique)
        ("/alcohol.html", "Alcohol", initAlcohol, aboutAlcohol)
        ("/heartbeat.html", "Heartbeat", initHeartbeat, aboutHeartbeat)
        ("/memory.html", "Memory", initMemory, aboutMemory)
        ("/babynames.html", "Babynames", initBabyNames, aboutBabyNames)
        ("/days.html", "Days", initDays, aboutDays)
        ("/equation.html", "Equation", initEquation, aboutEquation)
        ("https://twitter.com/peheje", "Contact", id, "")
    ]

let nav =
    urls
    |> List.mapi (fun i (url, name, _, about) ->
        let active = if window.location.pathname = url then "active" else ""
        let notLast = i < (urls |> List.length) - 1
        [
            Html.a
                [
                    prop.href url
                    prop.text name
                    prop.classes [ active ]
                    prop.title about
                ]
            if notLast then Html.span [ Html.text " | " ]
        ]
    )
    |> List.collect id
    |> Html.nav
    |> Render.htmlView

handleCompareRedirect ()

let site = urls |> List.tryFind (fun (url, _, _, _) -> url = window.location.pathname)
match site with
| Some (_, _, init, _) -> init ()
| None -> failwith "unknown site!"
(fromId "menu").innerHTML <- nav