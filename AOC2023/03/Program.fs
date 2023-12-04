open System

let rows =
    System.IO.File.ReadAllLines "C:\Users\peter\Repos\LearningFSharp\AOC2023\03\data"

let log x = printfn "%A" x

let map =
    rows
    |> Array.map (fun row -> row |> Seq.toArray |> Array.map id)

let isGear symbol = symbol = '*'

let isSymbol symbol =
    symbol |> System.Char.IsDigit |> not
    && symbol <> '.'

let maxX = map[0].Length - 1
let maxY = map.Length - 1

let hasNeighborSymbol x y =
    (x > 0 && map[y][x - 1] |> isSymbol)
    || (x < maxX && map[y][x + 1] |> isSymbol)
    || (y > 0 && map[y - 1][x] |> isSymbol)
    || (y < maxY && map[y + 1][x] |> isSymbol)
    || (x > 0 && y > 0 && map[y - 1][x - 1] |> isSymbol)
    || (y < maxY
        && x < maxX
        && map[y + 1][x + 1] |> isSymbol)
    || (y < maxY && x > 0 && map[y + 1][x - 1] |> isSymbol)
    || (y > 0 && x < maxX && map[y - 1][x + 1] |> isSymbol)

let part1 =
    seq {

        for (y, row) in map |> Array.indexed do

            let sb = System.Text.StringBuilder()
            let mutable valid = false

            for (x, c) in row |> Array.indexed do
                if c |> System.Char.IsDigit then
                    sb.Append c |> ignore

                    if hasNeighborSymbol x y then
                        valid <- true
                else
                    if sb.Length <> 0 && valid then
                        yield (sb.ToString() |> int)

                    valid <- false
                    sb.Clear() |> ignore

            if sb.Length <> 0 && valid then
                yield (sb.ToString() |> int)

    }
    |> Seq.sum

// Part 2
type Coordinate = { x: int; y: int }

type Gear = { coordinate: Coordinate }

type Number =
    { value: int
      coordinates: Coordinate seq }

let gears =
    seq {
        for (y, row) in map |> Array.indexed do
            for (x, c) in row |> Array.indexed do
                if c = '*' then
                    yield { coordinate = { x = x; y = y } }
    }

let numbers =
    seq {

        for (y, row) in map |> Array.indexed do

            let sb = Text.StringBuilder()
            let indices = Collections.Generic.List<Coordinate>()

            for (x, c) in row |> Array.indexed do
                if c |> System.Char.IsDigit then
                    sb.Append c |> ignore
                    indices.Add({ x = x; y = y })
                else
                    if sb.Length <> 0 then
                        yield
                            { value = sb.ToString() |> int
                              coordinates = indices }

                    indices.Clear()
                    sb.Clear() |> ignore

            if sb.Length <> 0 then
                yield
                    { value = sb.ToString() |> int
                      coordinates = indices }

    }

let isNeighbor x1 y1 x2 y2 =
    let xd = abs(x1-x2)
    let yd = abs (y1-y2)
    xd < 2 && yd < 2

let mutable sum = 0
for gear in gears do
    let gCoord = gear.coordinate
    
    let mutable numbersCollected = Collections.Generic.List<int>()
    let mutable countNextToGear = 0
    for number in numbers do
        
        if number.coordinates |> Seq.exists (fun numberCoord ->
            isNeighbor numberCoord.x numberCoord.y gCoord.x gCoord.y
        ) then
            countNextToGear <- countNextToGear + 1
            numbersCollected.Add (number.value)
    
    if countNextToGear = 2 then
        printfn "GEAR: %A N1 %A N2 %A" gCoord numbersCollected[0] numbersCollected[1]
        let power = numbersCollected[0] * numbersCollected[1]
        sum <- sum + power

    numbersCollected.Clear()

   