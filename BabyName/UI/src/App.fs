module App.Main

open Browser
open Feliz.ViewEngine

open BabyNames
open Compare
open Alcohol
open Heartbeat
open Unique
open Memory
open Days

let urls =
    [
        ("/compare.html", "Compare", initCompare)
        ("/unique.html", "Unique", initUnique)
        ("/alcohol.html", "Alcohol", initAlcohol)
        ("/heartbeat.html", "Heartbeat", initHeartbeat)
        ("/memory.html", "Memory", initMemory)
        ("/babynames.html", "Babynames", initBabyNames)
        ("/days.html", "Days", initDays)
        ("https://twitter.com/peheje", "Contact", id)
    ]

let nav =
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
    )
    |> List.collect id
    |> Html.nav
    |> Render.htmlView

let noticeExpiry = System.DateTimeOffset(2023, 04, 1, 0, 0, 0, System.TimeSpan.Zero)

let notice = 
    if System.DateTimeOffset.Now < noticeExpiry 
    then
        Html.p
            [
                prop.text "25/02/2023: Rewrote site in F# Fable, let me know if you see issues."
                prop.classes [ "notice" ]
                prop.id "notice"
            ] |> Render.htmlView
    else
        Html.none |> Render.htmlView

if window.location.pathname = "/compare/compare.html" then
    window.setTimeout (fun _ ->
        window.location.pathname <- "/compare.html"
    , 6000) |> ignore
else
    let site = urls |> List.tryFind (fun (url, _, _) -> url = window.location.pathname)
    match site with
    | Some (_, _, init) -> init ()
    | None -> failwith "unknown site!"
    (document.getElementById "menu").innerHTML <- notice + nav