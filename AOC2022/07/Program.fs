let path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/07/input"

let rows =
    System.IO.File.ReadAllLines(path)
    |> Array.filter (fun row -> row <> "$ ls")
    |> Array.skip 1
    |> Array.toList

let isChangeFolder (row: string) = row.StartsWith("$ cd") && row.EndsWith("..") |> not
let isFile (row: string) = System.Char.IsDigit(row[0])
let isFolder (row: string) = row.StartsWith("dir ")
let isGoBack (row: string) = row = "$ cd .."
let fileSize (row: string) = row.Split(" ")[0] |> int
let changedToFolderName (row: string) = row.Split("$ cd ")[1]
let folderName (row: string) = row.Split("dir ")[1]

type Folder =
    { Name: string
      mutable Parent: Folder option
      mutable Files: int list
      mutable Folders: Folder list }

let rec buildTree rows current =
    match rows with
    | row :: rest when row |> isFolder ->
        let folder =
            { Name = row |> folderName
              Parent = Some current
              Files = []
              Folders = [] }

        current.Folders <- folder :: current.Folders
        buildTree rest current
    | row :: rest when row |> isFile ->
        let size = row |> fileSize
        current.Files <- size :: current.Files
        buildTree rest current
    | row :: rest when row |> isChangeFolder ->
        let folderName = row |> changedToFolderName

        let folder =
            current.Folders
            |> List.find (fun f -> f.Name = folderName)

        buildTree rest folder
    | row :: rest when row |> isGoBack -> buildTree rest (Option.get current.Parent)
    | [] -> ()
    | _ -> failwith "Unhandled input"

let root =
    { Name = "/"
      Parent = None
      Files = []
      Folders = [] }

buildTree rows root

let rec folderSize folder =
    let sum = folder.Files |> List.sum

    folder.Folders
    |> List.fold (fun accumulator innerFolder -> accumulator + (folderSize innerFolder)) sum

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
