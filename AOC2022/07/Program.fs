let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/07/input"
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
let isGoBack (row: string) = row = "$ cd .."

type Directory = { Name: string; mutable Parent: Directory option; mutable Files: int list; mutable Folders: Directory list }
    
let rec buildGraph (rows: string list) (current: Directory): Directory =
    match rows with
    | row :: rest when row |> isDir ->
        let dirName = directoryName row
        let dir = {Name=dirName; Parent=current.Parent; Files = []; Folders = List.empty}
        current.Folders <- (dir :: current.Folders)
        buildGraph rest current

    | row :: rest when row |> isFile ->
        let size = row |> fileSize
        current.Files <- size :: current.Files
        buildGraph rest current
    
    | row :: rest when row |> isChangeDirectory ->
        let dirName = changedToDirectoryName row
        let dir = (current.Folders |> List.find (fun f -> f.Name = dirName))
        dir.Parent <- Some current
        buildGraph rest dir

    | row :: rest when row |> isGoBack ->
        buildGraph rest (Option.get current.Parent)

    | [] -> current

    | _ -> failwith "havent dont that part yet"


let root = {Name="/"; Parent=None; Files = []; Folders = List.empty}
buildGraph rows root

printfn "%A" root