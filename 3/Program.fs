let logs x = printfn "%A" x; x
let xs = System.IO.File.ReadAllLines "1.txt"
let xxs = xs |> Seq.transpose
let bin2dec s = System.Convert.ToInt32(s, 2)
let countWhere a = Seq.filter a >> Seq.length

let msb2dec c xxs =
    xxs
    |> Seq.map (countWhere (fun x -> x = c))
    |> Seq.map (fun c -> if c > (Seq.length xs) / 2 then "0" else "1")
    |> String.concat ""
    |> bin2dec

let gamma = xxs |> msb2dec '0'
let epsilon = xxs |> msb2dec '1'
let ans = gamma * epsilon