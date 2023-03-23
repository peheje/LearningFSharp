module Days

open System
open Html

let private isWeekend (time: DateTime) =
    time.DayOfWeek = DayOfWeek.Saturday || time.DayOfWeek = DayOfWeek.Sunday

let private formatDays totalDays =
    let weeks, days = Math.DivRem(totalDays, 7)
    let weeksText = if weeks = 1 then "1 week" else sprintf "%i weeks" weeks
    let daysText = if days = 1 then "1 day" else sprintf "%i days" days
    
    match weeks, days with
    | 0, 0 -> "None"
    | 0, _ -> daysText
    | _, 0 -> sprintf "%i days (%s)" totalDays weeksText
    | _, _ -> sprintf "%i days (%s and %s)" totalDays weeksText daysText

let initDays () =
    let start = (inputFromId "start-day")
    start.valueAsDate <- DateTime.Now

    let stop = (inputFromId "end-day")
    stop.valueAsDate <- DateTime.Now

    let addDaysBtn = (inputFromId "add-days-btn")

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
        let totalDaysEl = (fromId "total-duration")
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

    let addDays () =
        let addDaysInput = (inputFromId "add-days-input")
        let daysToAdd = addDaysInput.valueAsNumber
        stop.valueAsDate <- stop.valueAsDate.AddDays (daysToAdd)
        calculate ()

    start |> onChange calculate
    stop |> onChange calculate
    addDaysBtn |> onClick addDays

    calculate ()
