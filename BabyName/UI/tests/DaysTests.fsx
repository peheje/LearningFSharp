// Run with dotnet fsi tests/DaysTests.fsx
#load "../src/DaysCore.fs"

open System
open DaysCore

let assertEqual expected actual name =
    if expected <> actual then
        failwithf "%s: expected %A but got %A" name expected actual

let assertAlmostEqual expected actual tolerance name =
    if abs (expected - actual) >= tolerance then
        failwithf "%s: expected %A but got %A" name expected actual

let testLeapYear () =
    let (days, _), reverse = collectDays (DateTime(2024, 2, 27)) (DateTime(2024, 3, 1))
    assertEqual false reverse "reverse flag"
    assertEqual 4 (List.length days) "leap year day count"
    assertEqual 0 (List.filter isWeekend days |> List.length) "leap year weekend count"

let testWeekendCount () =
    let (days, _), _ = collectDays (DateTime(2024, 6, 7)) (DateTime(2024, 6, 10))
    assertEqual 4 (List.length days) "weekend total days"
    assertEqual 2 (List.filter isWeekend days |> List.length) "weekend days"

let testCollectTimeFraction () =
    let start = DateTime(2024, 6, 1, 12, 0, 0)
    let stop = DateTime(2024, 6, 2, 12, 0, 0)
    let total, weekend, _, reverse = collectTime start stop
    assertEqual false reverse "collectTime reverse"
    assertAlmostEqual 1.0 total 1e-6 "collectTime total"
    assertAlmostEqual 1.0 weekend 1e-6 "collectTime weekend"

let testLongRange () =
    let start = DateTime(2010, 1, 1)
    let stop = DateTime(2020, 12, 31)
    let (days, _), reverse = collectDays start stop
    assertEqual false reverse "long range reverse"
    let expectedDays = (stop - start).Days + 1
    assertEqual expectedDays (List.length days) "long range total days"

let testCollectTimeLongRange () =
    let start = DateTime(2019, 1, 1)
    let stop = DateTime(2021, 1, 1)
    let total, weekend, _, reverse = collectTime start stop
    assertEqual false reverse "long collectTime reverse"
    assertAlmostEqual 731.0 total 1e-6 "long collectTime total"
    let mutable dt = start
    let mutable count = 0

    while dt < stop do
        if isWeekend dt then
            count <- count + 1

        dt <- dt.AddDays 1.0

    assertAlmostEqual (float count) weekend 1e-6 "long collectTime weekend"

let testReverseOrderDays () =
    let start = DateTime(2024, 2, 27)
    let stop = DateTime(2024, 3, 1)
    let forward, _ = collectDays start stop
    let (revDays, revRatio), reverse = collectDays stop start
    assertEqual true reverse "reverse days flag"
    assertEqual (List.length (fst forward)) (List.length revDays) "reverse days count"
    assertAlmostEqual (snd forward) revRatio 1e-6 "reverse days month ratio"

let testReverseOrderTime () =
    let start = DateTime(2024, 2, 27, 6, 0, 0)
    let stop = DateTime(2024, 3, 1, 6, 0, 0)
    let totalF, weekendF, ratioF, _ = collectTime start stop
    let totalR, weekendR, ratioR, reverse = collectTime stop start
    assertEqual true reverse "reverse time flag"
    assertAlmostEqual totalF totalR 1e-6 "reverse time total"
    assertAlmostEqual weekendF weekendR 1e-6 "reverse time weekend"
    assertAlmostEqual ratioF ratioR 1e-6 "reverse time ratio"

let testCollectTimeMonthRatio () =
    let start = DateTime(2024, 3, 1, 0, 0, 0)
    let stop = DateTime(2024, 3, 2, 0, 0, 0)
    let _, _, monthRatio, _ = collectTime start stop
    assertAlmostEqual (1.0 / 31.0) monthRatio 1e-6 "collectTime month ratio"

let testCollectDaysMonthRatio () =
    let (days, monthRatio), _ =
        collectDays (DateTime(2024, 3, 1)) (DateTime(2024, 3, 2))

    assertEqual 2 (List.length days) "collectDays month ratio count"
    assertAlmostEqual (2.0 / 31.0) monthRatio 1e-6 "collectDays month ratio"

let testCrossMonthRatio () =
    let (days, monthRatioDays), _ =
        collectDays (DateTime(2024, 1, 31)) (DateTime(2024, 2, 1))

    assertEqual 2 (List.length days) "cross month days count"
    assertAlmostEqual ((1.0 / 31.0) + (1.0 / 29.0)) monthRatioDays 1e-6 "cross month days ratio"

    let _, _, monthRatioTime, _ =
        collectTime (DateTime(2024, 1, 31, 12, 0, 0)) (DateTime(2024, 2, 1, 12, 0, 0))

    assertAlmostEqual ((0.5 / 31.0) + (0.5 / 29.0)) monthRatioTime 1e-6 "cross month time ratio"

let run () =
    testLeapYear ()
    testWeekendCount ()
    testCollectTimeFraction ()
    testLongRange ()
    testCollectTimeLongRange ()
    testReverseOrderDays ()
    testReverseOrderTime ()
    testCollectTimeMonthRatio ()
    testCollectDaysMonthRatio ()
    testCrossMonthRatio ()
    printfn "All tests passed"

run ()
