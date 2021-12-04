let logs x = printfn "%A" x
let xs = System.IO.File.ReadAllLines "1.txt"
let xxs = xs |> Seq.transpose
let bin2dec s = System.Convert.ToInt32(s, 2)
let countWhere a = Seq.filter a >> Seq.length

// Part 1
let msb2dec c xxs =
    xxs
    |> Seq.map (countWhere (fun x -> x = c))
    |> Seq.map (fun x -> if x > (Seq.length xs) / 2 then "0" else "1")
    |> String.concat ""
    |> bin2dec

let gamma = xxs |> msb2dec '0'
let epsilon = xxs |> msb2dec '1'
let power = gamma * epsilon

// Part 2
let mostOfAtIndex i (xs: string array) =
    let zeroes = xs |> Seq.map (fun x -> x[i]) |> countWhere (fun x -> x = '0')
    if zeroes > Seq.length xs / 2 then '0' else '1'

let leastOfAtIndex i xs =
    if xs |> mostOfAtIndex i = '0' then '1' else '0'

let filterByCriteria strategy strings =
    let rec filter index (filteredItems: string array) =
        logs index
        logs filteredItems
        match Seq.length filteredItems with
        | 1 -> filteredItems
        | _ ->
            let most = filteredItems |> strategy index
            filter (index+1) (filteredItems |> Array.filter (fun x -> x[index] = most))
    (filter 0 strings)[0]

let oxygen = xs |> filterByCriteria mostOfAtIndex |> bin2dec
let co2 = xs |> filterByCriteria leastOfAtIndex |> bin2dec
oxygen * co2