open System.IO
open System.Text.RegularExpressions

let search = @"C:\Users\peter\Repos"
let target = "exception"
let ignoredFileTypes = [|".dll"; ".exe" |]
let ignoredFolders = [| ".git" |]

let filterEndsWith (ignore: string array) (xs: string array) =
    xs
    |> Array.filter (fun file -> ignore |> Array.exists file.EndsWith |> not)

let allFiles =
    Directory.EnumerateDirectories(search, "*", SearchOption.AllDirectories)
    |> Seq.toArray
    |> filterEndsWith ignoredFolders
    |> Seq.collect Directory.EnumerateFiles
    |> Seq.toArray

let readFileContent path = (path, File.ReadAllLines path)

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
    |> Array.map readFileContent
    |> Array.map (find target)
    |> Array.filter fileHasSearchResults

let dump x =
    let json = System.Text.Json.JsonSerializer.Serialize x
    File.WriteAllText("C:\Users\peter\Repos\Search\Searcher\dump.json", json)

printfn "dumping.."

dump result

let numberOfFiles = result |> Array.length
let numberOfResults = result |> Array.sumBy (fun (_, lines) -> lines |> Array.length)

printfn "found %i results in %i files with %s" numberOfResults numberOfFiles target

printfn "goodbye from F#!"
