module Compare

open Browser.Types
open Html

let private readCompareList id =
    let xs = (areaFromId id).value |> split '\n'
    let ignoreCase = (inputFromId "case-insensitive").checked

    xs
        |> Array.map (fun x -> if ignoreCase then x.ToLower() else x)
        |> Array.filter (fun x -> x.Trim() <> "")
        |> Array.distinct

let private setTextArea id countId xs =
    (areaFromId id).value <- xs |> join '\n'
    (fromId countId).textContent <- xs.Length |> string

let private compare () =
    let a = readCompareList "a"
    let b = readCompareList "b"

    let aSet = a |> Set.ofArray
    let bSet = b |> Set.ofArray

    setTextArea "a" "a-count" a
    setTextArea "b" "b-count" b

    let both = Set.intersect aSet bSet |> Set.toArray
    setTextArea "both" "both-count" both

    let onlyA = Set.difference aSet bSet |> Set.toArray
    setTextArea "only-a" "only-a-count" onlyA

    let onlyB = Set.difference bSet aSet |> Set.toArray
    setTextArea "only-b" "only-b-count" onlyB

let initCompare () =

    let compareBtn = fromId "compare-btn"

    compareBtn |> onClick compare
