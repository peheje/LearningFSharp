let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\07\input"
let rows = System.IO.File.ReadAllLines(path) |> Array.toList

let rowsFiltered =
    rows
    |> List.filter (fun row -> row.Contains("..") |> not)

let isChangeDirectory (row: string) = row.StartsWith("$ cd")

let isFile (row: string) = System.Char.IsDigit(row[0])

let isDir (row: string) = row.StartsWith("dir ")

let fileSize (row: string) = row.Split(" ")[0] |> int

let changedToDirectoryName (row: string) = row.Split("$ cd ")[1]

let directoryName (row: string) = row.Split("dir ")[1]

let rec group (rows: string list) dest =
    match rows with

    | row :: rest when row |> isChangeDirectory ->
        let dir = row |> changedToDirectoryName
        group rest ((dir, "0") :: dest)

    | row :: rest when row |> isFile ->
        let size = fileSize row
        let currentDirectory = dest |> List.head |> fst
        group rest ((currentDirectory, size |> string) :: dest)

    | row :: rest when row |> isDir ->
        let directoryName = row |> directoryName
        let currentDirectory = dest |> List.head |> fst
        group rest ((currentDirectory, directoryName) :: dest)

    | [] -> dest

    | _ :: rest -> group rest dest

let structure = group rowsFiltered []

let directFolderSizes =
    structure
    |> List.groupBy fst
    |> List.map (fun (dir, items) -> (dir, items |> List.filter (snd >> isFile)))
    |> List.map (fun (dir, sizes) -> (dir, sizes |> List.sumBy (snd >> int)))
    |> Map.ofList

let part1 =
    structure
    |> List.map (fun (dir, item) ->
        if item |> isFile then
            (dir, item |> int)
        else
            (dir, directFolderSizes |> Map.find item))
    |> List.groupBy fst
    |> List.map (fun (dir, sizes) -> (dir, sizes |> List.sumBy (snd >> int)))
