type Monkey = 
    { Number: int
      mutable Items: array<int64>
      Operation: (int64 -> int64)
      ToThrowIndex: (int64 -> int)
      mutable Inspected: int64
      TestDivisibleBy: int64 }

let remove (target: string) (source: string) = source.Replace(target, "")
let split (by: string) (source: string) = source.Split(by)

let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/kotlin/aoc2022/src/main/kotlin/input11.txt"
let monkeysRaw = System.IO.File.ReadAllText(path).Split("\n\n")

let monkeys = monkeysRaw |> Array.map (fun monkeyRaw ->
    let rows = monkeyRaw.Split("\n") |> Array.map (fun it -> it.Trim())
    let monkeyNumber = rows[0] |> remove "Monkey " |> remove ":" |> int
    let startingItems = rows[1] |> remove "Starting items: " |> split ", " |> Array.map int64
    let operation = rows[2] |> remove "Operation: new = old " |> split " "

    let op = match operation with
              | [|"*"; "old"|] -> (fun a -> a * a)
              | [|"*"; v|] -> (fun a -> a * int64 v)
              | [|"+"; v|] -> (fun a -> a + int64 v)
              | _ -> failwith "Unhandled operation"
    
    let testDivisibleBy = rows[3] |> remove "Test: divisible by " |> int64
    let ifTrue = rows[4] |> remove "If true: throw to monkey " |> int
    let ifFalse = rows[5] |> remove "If false: throw to monkey " |> int
    let throwToIndex = fun a -> if (a % testDivisibleBy = 0L) then ifTrue else ifFalse

    { Number = monkeyNumber; Items = startingItems; Operation = op; ToThrowIndex = throwToIndex; Inspected = 0L; TestDivisibleBy = testDivisibleBy })

let commonDivisor = monkeys |> Array.map (fun it -> it.TestDivisibleBy) |> Array.reduce (fun acc i -> acc * i)

let roundsTotal = 10000
let mutable round = 0
while round < roundsTotal do
    for monkey in monkeys do
        for i in 0 .. monkey.Items.Length - 1 do
            monkey.Inspected <- monkey.Inspected + 1L
            // monkey.Items[i] <-  monkey.Operation(monkey.Items[i]) / 3L // part 1
            monkey.Items[i] <- monkey.Operation(monkey.Items[i] % commonDivisor)
            let throwTo = monkey.ToThrowIndex monkey.Items[i]
            monkeys[throwTo].Items <- Array.append monkeys[throwTo].Items [|monkey.Items[i]|]
            
        monkey.Items <- [||]

    round <- round + 1

    if (round = 1 || round = 20 || round % 1000 = 0) then
        printfn "== After round %d ==" round
        for monkey in monkeys do
            printfn "Monkey %d inspected %d times." monkey.Number monkey.Inspected

let mostActive =
    monkeys
    |> Array.map (fun it -> it.Inspected)
    |> Array.sortDescending
    |> Array.take 2

printfn "%d" (mostActive[0] * mostActive[1])
