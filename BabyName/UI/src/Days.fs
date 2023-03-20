module Days

open System
open Html

let private isWeekend (time: DateTime) =
    time.DayOfWeek = DayOfWeek.Saturday || time.DayOfWeek = DayOfWeek.Sunday

let private pluralize word count =
    if count = 1 then word
    else word + "s"

let private formatDays days =
    let weeks, remainingDays = Math.DivRem(days, 7)
    let daysText = sprintf "%i %s" days (pluralize "day" days)
    let weeksText = sprintf "(%i %s" weeks (pluralize "week" weeks)
    let remainingWeekDaysText = sprintf " and %i %s)" remainingDays (pluralize "day" remainingDays)

    if days < 7 then
        daysText
    else
        if remainingDays = 0 then
            sprintf "%s %s)" daysText weeksText
        else
            sprintf "%s %s %s" daysText weeksText remainingWeekDaysText

let initDays () =
    let start = (inputFromId "start-day")
    start.valueAsDate <- DateTime.Now

    let stop = (inputFromId "end-day")
    stop.valueAsDate <- DateTime.Now

    let rec collectDays (cursor: DateTime) stop out =
        if cursor <= stop then
            collectDays (cursor.AddDays(1)) stop (cursor :: out)
        else
            out

    let validate () =
        try
            let _, _ = start.valueAsDate.Date, stop.valueAsDate.Date
            true
        with _ ->
            false

    let calculate () =
        let errorEl = (fromId "error")
        let totalDaysEl = (fromId "total-days")
        let weekendDaysEl = (fromId "weekend-days")

        if validate () then
            errorEl.textContent <- ""
            let days = collectDays start.valueAsDate stop.valueAsDate List.empty
            let daysCount = days |> List.length
            let weekendCount = days |> List.filter isWeekend |> List.length

            totalDaysEl.textContent <- (formatDays daysCount)
            weekendDaysEl.textContent <- (formatDays weekendCount)
        else
            errorEl.textContent <- "Error in date"
            totalDaysEl.textContent <- "-"
            weekendDaysEl.textContent <- "-"

    start |> onChange calculate
    stop |> onChange calculate

    calculate ()
