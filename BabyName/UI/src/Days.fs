module Days

open System
open Html

let private isWeekend (time: DateTime) =
    time.DayOfWeek = DayOfWeek.Saturday || time.DayOfWeek = DayOfWeek.Sunday

let private pluralize word count =
    if count = 1 then word
    else word + "s"

let private formatDays totalDays =
    let weeks, remainingDays = Math.DivRem(totalDays, 7)
    let daysText = sprintf "%i %s" totalDays (pluralize "day" totalDays)
    let weeksText = sprintf "(%i %s" weeks (pluralize "week" weeks)
    let remainingWeekDaysText = sprintf " and %i %s)" remainingDays (pluralize "day" remainingDays)

    if totalDays < 7 then
        daysText
    else
        if remainingDays = 0 then
            daysText + " " + weeksText + ")"
        else
            daysText + " " + weeksText + " " + remainingWeekDaysText

let initDays () =
    let startDayInput = (inputFromId "start-day")
    startDayInput.valueAsDate <- DateTime.Now

    let endDayInput = (inputFromId "end-day")
    endDayInput.valueAsDate <- DateTime.Now

    let rec collectDays (cursor: DateTime) stop out =
        if cursor < stop then
            collectDays (cursor.AddDays(1)) stop (cursor :: out)
        else
            out

    let calculate () =
        let start = startDayInput.valueAsDate
        let stop = endDayInput.valueAsDate
        let days = collectDays start stop List.empty
        
        let daysCount = days |> List.length
        let weekendCount = days |> List.filter isWeekend |> List.length
        
        let daysText = formatDays daysCount
        let weekendText = formatDays weekendCount

        (fromId "total-days").textContent <- daysText
        (fromId "weekend-days").textContent <- weekendText

    startDayInput |> onChange calculate
    endDayInput |> onChange calculate

    calculate ()
