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

let private daysToWeeks days =
    let weeks, remainingDays = Math.DivRem(days, 7)
    sprintf "%i weeks and %i days" weeks remainingDays
    
let initDays () =
    let startDayInput = (inputFromId "start-day")
    startDayInput.valueAsDate <- DateTime.Now

    let endDayInput = (inputFromId "end-day")
    endDayInput.valueAsDate <- DateTime.Now.AddDays(7)

    let calculate () =
        let addDay, days = counter ()
        let addWeekendDay, weekendDays = counter ()

        let mutable cursor = startDayInput.valueAsDate
        while cursor < endDayInput.valueAsDate do
            addDay ()
            if isWeekend cursor then addWeekendDay()
            cursor <- cursor.AddDays(1)

        (fromId "total-days").textContent <- (days() |> string)
        (fromId "weeks").textContent <- (daysToWeeks (days()))
        (fromId "weekend-days").textContent <- (weekendDays () |> string)

    (inputFromId "calculate") |> onClick calculate

    calculate ()
