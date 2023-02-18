module Compare

open Browser.Types
open Html

let private readInput id =
    let xs = (areaFromId id).value |> split '\n'
    let ignoreCase = (inputFromId "case-insensitive").checked

    let filtered = 
        xs
        |> Array.map (fun x -> if ignoreCase then x.ToLower() else x)
        |> Array.filter (fun x -> x.Trim() <> "")
        |> Array.distinct

    (filtered, filtered |> Set.ofArray)

let private setTextArea id countId xs =
    (areaFromId id).value <- xs |> join '\n'
    (fromId countId).textContent <- xs.Length |> string

let private compare () =
    let aList, aSet = readInput "a"
    let bList, bSet = readInput "b"

    setTextArea "a" "a-count" aList
    setTextArea "b" "b-count" bList

    let both = Set.intersect aSet bSet |> Set.toArray
    setTextArea "both" "both-count" both

    let onlyA = Set.difference aSet bSet |> Set.toArray
    setTextArea "only-a" "only-a-count" onlyA

    let onlyB = Set.difference bSet aSet |> Set.toArray
    setTextArea "only-b" "only-b-count" onlyB

let private random = System.Random()

let private randomize () =
    Array.init 10000 (fun _ -> random.Next(10000) |> string) |> setTextArea "a" "a-count"
    Array.init 10000 (fun _ -> random.Next(10000) |> string) |> setTextArea "b" "b-count"
    compare ()

let initCompare () =

    fromId "compare-btn" |> onClick compare
    fromId "random-btn" |> onClick randomize
