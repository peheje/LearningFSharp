module Compare

open Browser
open Html
open Browser.Types

let private setTextArea id countId xs =
    (areaFromId id).value <- xs |> join newline
    (fromId countId).textContent <- xs |> Array.length |> string

let private compareData () =
    let readInput id =
        let ignoreCase = (inputFromId "case-insensitive").checked
        let filtered = 
            (areaFromId id).value 
            |> split newline
            |> Array.map (fun x -> if ignoreCase then x.ToLower() else x)
            |> Array.filter (fun x -> x.Trim() <> "")
            |> Array.distinct
        (filtered, filtered |> Set.ofArray)

    let aList, aSet = readInput "a"
    let bList, bSet = readInput "b"
    let both = Set.intersect aSet bSet |> Set.toArray
    let onlyA = Set.difference aSet bSet |> Set.toArray
    let onlyB = Set.difference bSet aSet |> Set.toArray
    (aList, bList, both, onlyA, onlyB)

let private compare () =
    let (a, b, both, onlyA, onlyB) = compareData ()
    setTextArea "a" "a-count" a
    setTextArea "b" "b-count" b
    setTextArea "both" "both-count" both
    setTextArea "only-a" "only-a-count" onlyA
    setTextArea "only-b" "only-b-count" onlyB

let private random = System.Random()

let private randomize () =
    Array.init 10000 (fun _ -> random.Next(10000) |> string) |> setTextArea "a" "a-count"
    Array.init 10000 (fun _ -> random.Next(10000) |> string) |> setTextArea "b" "b-count"
    compare ()

let private download () =
    let getValidSeparator () =
        let source = (areaFromId "a").value + (areaFromId "b").value
        [|"|"; ";"; ","|] |> Array.tryFind (fun separator -> source |> contains separator |> not)

    let takeOrEmpty source index =
        match source |> Array.tryItem index with | None -> "" | Some v -> v

    match getValidSeparator () with
        | None -> window.alert "Download failed. Input already includes separator values | ; ,"
        | Some separator ->
            let (a, b, both, onlyA, onlyB) = compareData ()
            let mutable data = "Left" + separator + "Right" + separator + "In both" + separator + "Only in left" + separator + "Only in right\n"
            
            let size = max a.Length b.Length
            for i in 0..size - 1 do
                data <- data + takeOrEmpty a i
                data <- data + separator
                data <- data + takeOrEmpty b i
                data <- data + separator
                data <- data + takeOrEmpty both i
                data <- data + separator
                data <- data + takeOrEmpty onlyA i
                data <- data + separator
                data <- data + takeOrEmpty onlyB i
                data <- data + "\n"
            
            let downloadBtn = (fromId "download-btn") :?> HTMLLinkElement
            downloadBtn.href <- "data:text/plain;charset=UTF-8," + window.encodeURIComponent(data)

let initCompare () =
    fromId "compare-btn" |> onClick compare
    fromId "random-btn" |> onClick randomize
    fromId "download-btn" |> onClick download
