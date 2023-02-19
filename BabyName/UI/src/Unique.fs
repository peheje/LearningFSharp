module Unique

open Html

let private readInput id ignoreCase =
    let areaValue = if ignoreCase then (areaFromId id).value.ToLower()
                    else (areaFromId id).value
    areaValue
        |> split newline
        |> Array.filter (fun x -> x.Trim() <> "")

let private getDuplicates xs =
    let o = System.Collections.Generic.HashSet<string>()

    xs |> Array.choose (fun x ->
        if o.Contains x then
            o.Add x |> ignore
            Some x
        else
            o.Add x |> ignore
            None
    )

let private findDuplicates () =
    let ignoreCase = (inputFromId "case-insensitive").checked
    let a = readInput "original" ignoreCase

    setTextArea "original" "original-count" a
    setTextArea "unique" "unique-count" (a |> Array.distinct)
    let duplicates = getDuplicates a
    setTextArea "duplicates" "duplicates-count" duplicates

let initUnique () =
    fromId "find-duplicates-btn" |> onClick findDuplicates