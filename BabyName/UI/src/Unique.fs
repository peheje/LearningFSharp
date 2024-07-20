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

let aboutUnique = "Tool to analyze a list of strings, identifying both unique entries and duplicates. Input your list, and receive output displaying unique items and repeated ones. For example, inputting 'a' 3 times will show 'a' as a unique entry, while also listing it twice under duplicates."