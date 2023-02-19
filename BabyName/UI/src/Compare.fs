module Compare

open Browser
open Html
open Browser.Types

let private compareData () =
    let readInput id =
        let ignoreCase = (inputFromId "case-insensitive").checked
        let areaValue = if ignoreCase then (areaFromId id).value.ToLower()
                        else (areaFromId id).value
        let filtered = 
            areaValue
            |> split newline
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

let private download (event: Event) =
    let getValidSeparator () =
        let source = (areaFromId "a").value + (areaFromId "b").value
        [|"|"; ";"; ","|] |> Array.tryFind (fun separator -> source |> contains separator |> not)

    let takeOrEmpty source index =
        match source |> Array.tryItem index with | None -> "" | Some v -> v

    match getValidSeparator () with
        | None ->
            event.preventDefault()
            window.alert "Download failed. Input already includes separator values | ; ,"
        | Some separator ->
            let (a, b, both, onlyA, onlyB) = compareData ()
            let compareData = [|a; b; both; onlyA; onlyB|]
            
            let size = max (a |> Array.length) (b |> Array.length)
            let mutable data = "Left" + separator + "Right" + separator + "In both" + separator + "Only in left" + separator + "Only in right\n"
            for i in 0..size - 1 do
                for item in compareData do
                    data <- data + takeOrEmpty item i + separator
                data <- data + "\n"
            
            let downloadBtn = (fromId "download-btn") :?> HTMLLinkElement
            downloadBtn.href <- "data:text/plain;charset=UTF-8," + window.encodeURIComponent(data)

let initCompare () =
    fromId "compare-btn" |> onClick compare
    fromId "random-btn" |> onClick randomize
    fromId "download-btn" |> onClickEvent download
