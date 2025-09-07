module DaysCore

open System

let isWeekend (time: DateTime) =
    time.DayOfWeek = DayOfWeek.Saturday || time.DayOfWeek = DayOfWeek.Sunday

let collectTime (start: DateTime) stop =
    let start, stop, reverse =
        if start > stop then stop, start, true else start, stop, false

    let mutable cursor = DateTime(start.Year, start.Month, start.Day)
    let mutable totalDays = 0.0
    let mutable weekendDays = 0.0
    let mutable monthRatio = 0.0

    while cursor <= stop.Date do
        let dayEnd = cursor.AddDays(1.0)
        let overlapStart = if cursor < start then start else cursor
        let overlapEnd = if dayEnd > stop then stop else dayEnd
        let fraction = (overlapEnd - overlapStart).TotalDays
        if fraction > 0.0 then
            totalDays <- totalDays + fraction
            if isWeekend cursor then weekendDays <- weekendDays + fraction
            let daysInMonth = DateTime.DaysInMonth(cursor.Year, cursor.Month) |> float
            monthRatio <- monthRatio + fraction / daysInMonth
        cursor <- cursor.AddDays 1.0

    (totalDays, weekendDays, monthRatio, reverse)

let collectDays (start: DateTime) stop =
    let rec collectDays' (cursor: DateTime) stop collectedDays collectedMonths =
        if cursor <= stop then
            let daysInMonth = DateTime.DaysInMonth(cursor.Year, cursor.Month) |> float
            let monthAddition = 1.0 / daysInMonth
            collectDays' (cursor.AddDays 1.0) stop (cursor :: collectedDays) (collectedMonths + monthAddition)
        else
            (collectedDays, collectedMonths)

    if start > stop then
        (collectDays' stop start [] 0.0, true)
    else
        (collectDays' start stop [] 0.0, false)
