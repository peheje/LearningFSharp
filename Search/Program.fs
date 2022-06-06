open System.IO
open System.Text.RegularExpressions

let dump x =
    let json = System.Text.Json.JsonSerializer.Serialize x
    File.WriteAllText("C:\Users\peter\Repos\LearningFSharp\Search\dump.json", json)

let search = @"C:\Users"
let target = "Master"
let ignoredFileTypes = [|".dll"; ".exe"; ".db"; ".pdf" |]
let ignoredFolders = [| ".git" |]

let filterEndsWith (ignore: string array) (xs: string array) =
    xs
    |> Array.filter (fun file -> ignore |> Array.exists file.EndsWith |> not)

let rec getAllDirectories path =
    try
        let dirs = Directory.GetDirectories(path)
        let rest = dirs |> Array.collect getAllDirectories
        Array.append dirs rest
    with
        | :? System.UnauthorizedAccessException ->
            printfn "skipping folder %s no access" path
            Array.empty

let rec getAllFiles path =
    try
        Directory.GetFiles path
    with
        | :? System.UnauthorizedAccessException ->
            printfn "skipping file %s no access" path
            Array.empty
        | :? System.AggregateException as aggregateException ->
            printfn "failed with AggregateException %s" aggregateException.Message
            Array.empty
        | :? IOException as ioException ->
            printfn "failed with IOException %s" ioException.Message
            Array.empty

let allFiles =
    getAllDirectories search
    |> filterEndsWith ignoredFolders
    |> Array.collect getAllFiles

let readFileContent path =

    try
        (path, File.ReadAllLines path)
    with
        | :? IOException as ioException ->
            printfn "failed with IOException %s" ioException.Message
            (path, [||])

let count target line =
    Regex.Matches(line, target, RegexOptions.IgnoreCase).Count

type Result = { rowIdx: int; line: string; count: int }

let find target file =
    let path, lines = file

    let found =
        lines
        |> Array.indexed
        |> Array.map (fun (i, line) -> { rowIdx = i; count = line |> count target; line = line })
        |> Array.filter (fun result -> result.count > 0)

    (path, found)

let fileHasSearchResults (_, rows) = rows |> Array.length > 0

let result =
    allFiles
    |> filterEndsWith ignoredFileTypes
    |> Array.map (fun file -> readFileContent file |> find target)
    |> Array.filter fileHasSearchResults

printfn "dumping.."

dump result

let numberOfFiles = result |> Array.length
let numberOfResults = result |> Array.sumBy (fun (_, lines) -> lines |> Array.length)

printfn "found %i results in %i files with %s" numberOfResults numberOfFiles target

printfn "goodbye from F#!"
