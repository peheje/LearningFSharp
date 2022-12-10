let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\07\input"
let rows =
    System.IO.File.ReadAllLines(path)
    |> Array.filter (fun row -> row.StartsWith("$ ls") |> not)
    |> Array.skip 1
    |> Array.toList

let isChangeDirectory (row: string) =
    row.StartsWith("$ cd") && row.Contains("..") |> not

let isFile (row: string) = System.Char.IsDigit(row[0])
let isDir (row: string) = row.StartsWith("dir ")
let fileSize (row: string) = row.Split(" ")[0] |> int
let changedToDirectoryName (row: string) = row.Split("$ cd ")[1]
let directoryName (row: string) = row.Split("dir ")[1]


type Directory = { Name: string; Parent: string; Files: int list; Folders: Directory list }

let rec buildGraph (rows: string list) (current: Directory): Directory =
    match rows with
    | row :: rest when row |> isDir ->
        let dirName = directoryName row
        let dir = {Name=dirName; Parent=current.Name; Files = []; Folders = []}
        let current = { current with Folders = dir :: current.Folders }
        buildGraph rest current

    | row :: rest when row |> isFile ->
        let size = row |> fileSize
        let current = { current with Files = size :: current.Files }
        buildGraph rest current
    
    | row :: rest 

    | [] -> current
    
    | _ -> failwith "havent dont that part yet"


let g = buildGraph rows {Name="/"; Parent=""; Files = []; Folders = []}

printfn "%A" g