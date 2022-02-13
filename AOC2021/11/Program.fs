let lines = System.IO.File.ReadAllLines("data.txt")

let octos =
    lines
    |> Array.map (fun row -> [|for x in row -> int (string x)|])
    |> Array.collect id

let size = lines |> Array.length
let length = octos |> Array.length

let toIndex row col = row * size + col
let fromIndex index = (index/size, index % size)

let anyFlashing data =
    data |> Array.exists(fun x -> x > 9)

let surroundingIndices index =
    let (row, col) = fromIndex index
    let neighbors = [|(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)|]
    [|for (ri, ci) in neighbors -> toIndex (ri + row) (ci + col)|]
    |> Array.filter (fun index -> index >= 0 && index < length)

let mutable flashes = 0

for _ in 1..99 do
    for i in 0..length-1 do
        octos[i] <- octos[i] + 1

    while anyFlashing octos do
        for i in 0..length-1 do
            if octos[i] > 9 then
                flashes <- flashes + 1
                octos[i] <- -1
                for ni in surroundingIndices i do
                    if octos[ni] <> -1 then
                        octos[ni] <- octos[ni] + 1

    for i in 0..length-1 do
        if octos[i] = -1 then
            octos[i] <- 0

printfn "%i" flashes