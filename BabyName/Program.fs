open System

let allPath = "C:\\Users\\peter\\Repos\\LearningFSharp\\BabyName\\baby-names.csv"
let likedPath = "C:\\Users\\peter\\Repos\\LearningFSharp\\BabyName\\liked"
let dislikedPath = "C:\\Users\\peter\\Repos\\LearningFSharp\\BabyName\\disliked"
let all = IO.File.ReadAllLines(allPath)
let liked = IO.File.ReadAllLines(likedPath)
let disliked = IO.File.ReadAllLines(dislikedPath)

let isYes (input: string) =
    if input = "" then false
    else String.Equals(input[0] |> string, "y", StringComparison.OrdinalIgnoreCase)

let babyNames =
    all
    |> Array.map (fun row ->
        let s = row.Split(",")
        (s[1].Trim('"'), s[3].Trim('"'))
    )
    |> Array.filter (fun (_, gender) -> gender = "girl")
    |> Array.map (fun (name, _) -> name)
    |> Array.distinct
    |> Array.except liked
    |> Array.except disliked

IO.File.WriteAllLines("C:\\Users\\peter\\Repos\\LearningFSharp\\BabyName\\out", babyNames)

for babyName in babyNames do
    printfn "Do you like %s?" babyName
    let answer = Console.ReadLine()
    if answer |> isYes then
        IO.File.AppendAllLines(likedPath, [babyName])
    else
        IO.File.AppendAllLines(dislikedPath, [babyName])
    printfn ""