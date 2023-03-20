module Days

open System
open Html

let private isWeekend (time: DateTime) =
    match time.DayOfWeek with
    | DayOfWeek.Saturday | DayOfWeek.Sunday -> true
    | _ -> false

let private counter () =
    let mutable c = 0
    (fun () -> c <- c + 1), (fun () -> c)

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

    let calculate () =
        let addDay, days = counter ()
        let addWeekendDay, weekendDays = counter ()

        let mutable cursor = startDayInput.valueAsDate
        while cursor < endDayInput.valueAsDate do
            addDay ()
            if isWeekend cursor then addWeekendDay()
            cursor <- cursor.AddDays(1)

        (fromId "total-days").textContent <- (days () |> formatDays)

        let pluralizedWeekend = pluralize "day" (weekendDays ())
        (fromId "weekend-days").textContent <- ((weekendDays () |> string) + " weekend " + pluralizedWeekend)

    startDayInput |> onChange calculate
    endDayInput |> onChange calculate

    calculate ()
