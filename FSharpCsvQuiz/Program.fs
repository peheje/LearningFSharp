open Microsoft.VisualBasic.FileIO
open System.Threading.Tasks

let ask () =
    task { return! Task.Run(fun _ -> System.Console.ReadLine()) }

let startTimer seconds =
    task {
        do! Task.Delay(seconds * 1000)
        printfn "\nSorry, time ran out."
        return ""
    }

let readAllCsvRows (path: string) =
    let rec loop (parser: TextFieldParser) output =
        match parser.EndOfData with
        | false ->
            let fields = parser.ReadFields()
            loop parser (fields :: output)
        | true ->
            output
            |> List.rev
            |> List.map (fun x -> {| question = x[0]; answer = x[1] |})

    let parser = new TextFieldParser(path)
    parser.Delimiters <- [| "," |]
    parser.HasFieldsEnclosedInQuotes <- true
    Task.Run(fun _ -> loop parser [])

let play () =
    task {
        let! rows = readAllCsvRows "/Users/phj/Code/FSharpCsvQuiz/input.csv"
        let totalRows = rows |> Seq.length

        let mutable points = 0
        let mutable rowIdx = 0
        let timerTask = startTimer 10

        while not timerTask.IsCompleted && rowIdx < totalRows do
            let row = rows[rowIdx]
            rowIdx <- rowIdx + 1

            printfn "%s" row.question

            let askTask = ask ()
            let! finishedTask = Task.WhenAny([ askTask; timerTask ])

            if finishedTask = askTask then
                let! guess = finishedTask

                if guess = row.answer then
                    printfn "correct"
                    points <- points + 1
                else
                    printfn "incorrect"

        printfn "you had %i/%i correct" points rows.Length

        return rows
    }

let main () =
    task { return! play () }
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> ignore

main ()
