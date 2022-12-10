let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/07/input"

let rows =
    System.IO.File.ReadAllLines(path)
    |> Array.filter (fun row -> row <> "$ ls")
    |> Array.skip 1
    |> Array.toList

let isChangeDirectory (row: string) =
    row.StartsWith("$ cd")
    && row.EndsWith("..") |> not

let isFile (row: string) = System.Char.IsDigit(row[0])
let isDir (row: string) = row.StartsWith("dir ")
let isGoBack (row: string) = row = "$ cd .."
let fileSize (row: string) = row.Split(" ")[0] |> int
let changedToDirectoryName (row: string) = row.Split("$ cd ")[1]
let directoryName (row: string) = row.Split("dir ")[1]

type Directory =
    { Name: string
      mutable Parent: Directory option
      mutable Files: int list
      mutable Folders: Directory list }

let rec buildTree rows current =
    match rows with
    | row :: rest when row |> isDir ->
        let dir =
            { Name = directoryName row
              Parent = Some current
              Files = []
              Folders = [] }

        current.Folders <- (dir :: current.Folders)
        buildTree rest current
    | row :: rest when row |> isFile ->
        let size = row |> fileSize
        current.Files <- size :: current.Files
        buildTree rest current
    | row :: rest when row |> isChangeDirectory ->
        let dirName = changedToDirectoryName row

        let dir =
            (current.Folders
             |> List.find (fun f -> f.Name = dirName))

        dir.Parent <- Some current
        buildTree rest dir
    | row :: rest when row |> isGoBack -> buildTree rest (Option.get current.Parent)
    | [] -> current
    | _ -> failwith "Unhandled input"

let root =
    { Name = "/"
      Parent = None
      Files = []
      Folders = [] }

let _ = buildTree rows root

let rec folderSize root =
    let sum = root.Files |> List.sum

    root.Folders
    |> List.fold (fun state value -> state + (folderSize value)) sum

let rec folderSizes root =
    seq {
        for f in root.Folders do
            yield! (folderSizes f)

        folderSize root
    }

let sizes = folderSizes root

let part1 =
    sizes
    |> Seq.filter (fun s -> s <= 100000)
    |> Seq.sum

printfn "part1: %i" part1

let rootSize = folderSize root
let left = 70000000 - rootSize
let missing = 30000000 - left

let part2 =
    sizes
    |> Seq.filter (fun size -> size >= missing)
    |> Seq.min

printfn "part2: %i" part2