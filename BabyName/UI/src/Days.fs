module Days

open System
open Html
open Browser.Types

let private isWeekend (time: DateTime) =
    time.DayOfWeek = DayOfWeek.Saturday || time.DayOfWeek = DayOfWeek.Sunday

let private formatMonth (monthRatio: float) =
    sprintf "%.2f months" monthRatio

let private formatYear (monthRatio: float) =
    let years = monthRatio / 12.0
    if years <= 1 then years |> sprintf "%.3f year"
    else years |> sprintf "%.3f years"

let private formatDays totalDays =
    let weeks, days = Math.DivRem(totalDays, 7)
    let weeksText = if weeks = 1 then "1 week" else sprintf "%i weeks" weeks
    let daysText = if days = 1 then "1 day" else sprintf "%i days" days

    match weeks, days with
    | 0, 0 -> "None"
    | 0, _ -> sprintf "%s" daysText
    | _, 0 -> sprintf "%i days (%s)" totalDays weeksText
    | _, _ -> sprintf "%i days (%s and %s)" totalDays weeksText daysText

let private saveDate key (date: DateTime) =
    setLocalStorage key (date.ToString("O"))

let private loadDate key (el: HTMLInputElement) =
    match getLocalStorageOrEmpty key with
    | "" -> el.valueAsDate <- DateTime.Now
    | x ->
        let saved = DateTime.Parse(x)
        el.valueAsDate <- saved.ToLocalTime()

let initDays () =
    let start = inputFromId "start-day"
    let stop = inputFromId "end-day"

    loadDate "start-day" start
    loadDate "end-day" stop

    let collectDays (start: DateTime) stop =
        let rec collectDays' (cursor: DateTime) stop collectedDays collectedMonths =
            if cursor <= stop then
                let daysInMonth = DateTime.DaysInMonth(cursor.Year, cursor.Month) |> float
                let monthAddition = 1.0 / daysInMonth
                collectDays' (cursor.AddDays 1) stop (cursor :: collectedDays) (collectedMonths + monthAddition)
            else
                (collectedDays, collectedMonths)
        
        if start > stop then
            (collectDays' stop start List.empty 0.0, true)
        else
            (collectDays' start stop List.empty 0.0, false)

    let validate () =
        try
            start.valueAsDate.Year <= 9000 && stop.valueAsDate.Year <= 9000
        with _ ->
            false

    let calculate () =
        let errorEl = fromId "error"
        let totalDaysEl = fromId "total-duration"
        let weekendDaysEl = fromId "weekend-days"
        let monthsEl = fromId "months"
        let yearsEl = fromId "years"

        if validate () then
            saveDate "start-day" start.valueAsDate
            saveDate "end-day" stop.valueAsDate
            
            errorEl.textContent <- ""
            let (days, monthRatio), reverse = collectDays start.valueAsDate stop.valueAsDate
            let daysCount = days |> List.length
            let weekendCount = days |> List.filter isWeekend |> List.length

            totalDaysEl.textContent <- (if reverse then "-" else "") + (formatDays daysCount)
            weekendDaysEl.textContent <- formatDays weekendCount
            monthsEl.textContent <- formatMonth monthRatio
            yearsEl.textContent <- (if reverse then "-" else "") + formatYear monthRatio
        else
            errorEl.textContent <- "Error in date"
            totalDaysEl.textContent <- "-"
            weekendDaysEl.textContent <- "-"
            monthsEl.textContent <- "-"
            yearsEl.textContent <- "-"

    let addDays () =
        let addDaysInput = inputFromId "add-days-input"
        let daysToAdd = addDaysInput.valueAsNumber
        stop.valueAsDate <- stop.valueAsDate.AddDays (daysToAdd)
        calculate ()

    start |> onChangeWithCooldown 1000<ms> calculate
    stop |> onChangeWithCooldown 1000<ms> calculate
    (inputFromId "add-days-btn") |> onClick addDays

    calculate ()

let aboutDays = "Calculate total days, months, years, and weekend days between two dates (inclusive) with this tool. Will take leap days into account. Just input start and end dates for a quick time interval breakdown. Bonus: add extra days to the end date for extended planning."