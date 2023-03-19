module Days

open System

let midday = 12
let start = DateTimeOffset(2023, 04, 1, midday, 0, 0, TimeSpan.Zero)
let stop = start.AddDays (7.0 * 12.0)
//let stop = DateTimeOffset(2023, 05, 1, midday, 0, 0, TimeSpan.Zero)

let isWeekend (time: DateTimeOffset) =
    match time.DayOfWeek with
    | DayOfWeek.Saturday | DayOfWeek.Sunday -> true
    | _ -> false

let counter () =
    let mutable c = 0
    (fun () -> c <- c + 1), (fun () -> c)

let addDay, days = counter ()
let addWeekendDay, weekendDays = counter ()
let addHoliday, holiday = counter ()

let mutable cursor = start
while cursor < stop do
    addDay ()
    if isWeekend cursor then addWeekendDay()

    cursor <- cursor.AddDays(1)

printfn "%A" (days ())
printfn "%A" (weekendDays ())
