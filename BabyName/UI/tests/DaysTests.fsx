#load "../src/DaysCore.fs"
open System
open DaysCore

let assertEqual expected actual name =
    if expected <> actual then
        failwithf "%s: expected %A but got %A" name expected actual

let testLeapYear () =
    let (days, _), reverse = collectDays (DateTime(2024,2,27)) (DateTime(2024,3,1))
    assertEqual false reverse "reverse flag"
    assertEqual 4 (List.length days) "leap year day count"
    assertEqual 0 (List.filter isWeekend days |> List.length) "leap year weekend count"

let testWeekendCount () =
    let (days, _), _ = collectDays (DateTime(2024,6,7)) (DateTime(2024,6,10))
    assertEqual 4 (List.length days) "weekend total days"
    assertEqual 2 (List.filter isWeekend days |> List.length) "weekend days"

let testCollectTimeFraction () =
    let start = DateTime(2024,6,1,12,0,0)
    let stop = DateTime(2024,6,2,12,0,0)
    let total, weekend, _, reverse = collectTime start stop
    assertEqual false reverse "collectTime reverse"
    assertEqual 1.0 total "collectTime total"
    assertEqual 1.0 weekend "collectTime weekend"

let testLongRange () =
    let start = DateTime(2010,1,1)
    let stop = DateTime(2020,12,31)
    let (days, _), reverse = collectDays start stop
    assertEqual false reverse "long range reverse"
    let expectedDays = (stop - start).Days + 1
    assertEqual expectedDays (List.length days) "long range total days"

let testCollectTimeLongRange () =
    let start = DateTime(2019,1,1)
    let stop = DateTime(2021,1,1)
    let total, weekend, _, reverse = collectTime start stop
    assertEqual false reverse "long collectTime reverse"
    assertEqual 731.0 total "long collectTime total"
    let mutable dt = start
    let mutable count = 0
    while dt < stop do
        if isWeekend dt then count <- count + 1
        dt <- dt.AddDays 1.0
    assertEqual (float count) weekend "long collectTime weekend"

let run () =
    testLeapYear()
    testWeekendCount()
    testCollectTimeFraction()
    testLongRange()
    testCollectTimeLongRange()
    printfn "All tests passed"

run()
