// Common
let xs = System.IO.File.ReadAllLines "3.txt"
let bin2dec s = System.Convert.ToInt32(s, 2)
let countWhere a = Seq.filter a >> Seq.length

// Part 1
let msb2dec c xxs =
    xxs
    |> Seq.map (countWhere ((=) c))
    |> Seq.map (fun x -> if x > (Seq.length xs) / 2 then "0" else "1")
    |> String.concat ""
    |> bin2dec

let xxs = xs |> Seq.transpose
let gamma = xxs |> msb2dec '0'
let epsilon = xxs |> msb2dec '1'
let power = gamma * epsilon |> printfn "%A"

// Part 2
let mostOfAtIndex i (xs: string array) =
    if xs |> countWhere (fun x -> x[i] = '0') > Seq.length xs / 2 then '0' else '1'

let leastOfAtIndex i xs =
    if xs |> mostOfAtIndex i = '0' then '1' else '0'

let filterByCriteria criteria strings =
    let rec filter i (filtered: string array) =
        if Seq.length filtered = 1 then filtered
        else
            let target = filtered |> criteria i
            filter (i + 1) (filtered |> Array.filter (fun x -> x[i] = target))
    (filter 0 strings)[0]

let oxygen = xs |> filterByCriteria mostOfAtIndex |> bin2dec
let co2 = xs |> filterByCriteria leastOfAtIndex |> bin2dec
let lifeSupport = oxygen * co2 |> printfn "%A"