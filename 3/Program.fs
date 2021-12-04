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
    let zeroes =
        xs
        |> Seq.map (fun x -> x[i])
        |> countWhere (fun x -> x = '0')
    if zeroes > Seq.length xs / 2 then '0' else '1'

let leastOfAtIndex i xs =
    if xs |> mostOfAtIndex i = '0' then '1' else '0'

let mutable oxygen = xs
let mutable co2 = xs
for i in 0 .. Seq.length xs[0] - 1 do
    if Seq.length oxygen > 1 then
        let most = oxygen |> mostOfAtIndex i
        oxygen <- oxygen |> Array.filter (fun x -> x[i] = most)

    if Seq.length co2 > 1 then
        let least = co2 |> leastOfAtIndex i
        co2 <- co2 |> Array.filter (fun x -> x[i] = least)

(bin2dec oxygen[0]) * (bin2dec co2[0]) |> logs