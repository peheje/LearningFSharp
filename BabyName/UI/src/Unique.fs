module Unique

open Html

let private readInput id ignoreCase =
    let areaValue =
        if ignoreCase then
            (areaFromId id).value.ToLower ()
        else
            (areaFromId id).value

    areaValue |> split newline |> Array.filter (fun x -> x.Trim() <> "")

let private getDuplicates xs =
    xs
    |> Array.countBy id
    |> Array.map (fun (x, count) -> Array.init (count - 1) (fun _ -> x))
    |> Array.collect id

let private findDuplicates () =
    let ignoreCase = (inputFromId "case-insensitive").checked
    let a = readInput "original" ignoreCase

    setTextArea "original" "original-count" a
    setTextArea "unique" "unique-count" (a |> Array.distinct)
    let duplicates = getDuplicates a
    setTextArea "duplicates" "duplicates-count" duplicates

let initUnique () =
    fromId "find-duplicates-btn" |> onClick findDuplicates
