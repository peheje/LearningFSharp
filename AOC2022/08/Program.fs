open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

[<MemoryDiagnoser>]
type MyBenchmark() =
    
    let mutable xxs: int array array = Array.empty
    
    let moveUp x y =
        seq { for i in y .. -1 .. 0 -> xxs[i][x] }

    let moveDown x y =
        seq { for i in y .. xxs.Length - 1 -> xxs[i][x] }

    let moveLeft x y =
        seq { for i in x .. -1 .. 0 -> xxs[y][i] }

    let moveRight x y =
        seq { for i in x .. xxs[0].Length - 1 -> xxs[y][i] }

    let scenic treesInDirection x y =
        let origin = xxs[y][x]

        treesInDirection x y
        |> Seq.skip 1
        |> Seq.scan
            (fun (stop, count) tree ->
                match (stop, tree) with
                | true, _ -> (true, false)
                | _, v when v >= origin -> (true, true)
                | _ -> (false, true))
            (false, false)
        |> Seq.skip 1
        |> Seq.takeWhile snd
        |> Seq.length

    let score x y =
        (scenic moveUp x y)
        * (scenic moveDown x y)
        * (scenic moveLeft x y)
        * (scenic moveRight x y)
    
    [<GlobalSetup>]
    member this.GlobalSetup() =
        let path = "C:\Users\peter\Repos\LearningFSharp\AOC2022\08\data.txt"

        xxs <-
            System.IO.File.ReadAllLines path
            |> Array.map Array.ofSeq
            |> Array.map (Array.map (System.Char.GetNumericValue >> int))
    
    [<Benchmark>]
    member this.BenchmarkMethod() = 
        let best =
            xxs
            |> Array.mapi (fun y vs -> vs |> Array.mapi (fun x _ -> score x y))
            |> Array.collect id
            |> Array.max
        printfn "%i" best

BenchmarkRunner.Run<MyBenchmark>() |> ignore
