module Program

open System
open System.IO
open Interpolation

let readPoint () =
    let point = Console.ReadLine().Split(" ")
    let x = point[0] |> float
    let y = point[1] |> float
    (x, y)

let seqCreate someSeq point = Seq.append someSeq (Seq.singleton point)

let culculateAndPrint approxName someSeq step approx =
    let result = approx someSeq (fst (Seq.head someSeq)) step
    printfn "%s\n" approxName
    for pair in result do
        printfn "%A" pair

let rec doWhileRecursively step currentPointsForLinear currentPointsForNewton =
    culculateAndPrint "Linear" currentPointsForLinear step linearInterpolation
    culculateAndPrint "Newton" currentPointsForNewton step newtonInterpolation
    
    let newPoint = readPoint ()
    let newSeqForLinear= seqCreate currentPointsForLinear newPoint
    let newSeqForNewton= seqCreate currentPointsForNewton newPoint
    doWhileRecursively step (Seq.tail newSeqForLinear) (Seq.tail newSeqForNewton)

[<EntryPoint>]
let main (args) = 
    match Array.length args with
    | 0 ->
        printf "Step is: "
        let step = Console.ReadLine() |> float

        let firstPoint = readPoint ()
        let secondPoint = readPoint ()

        let someSeq = seqCreate (Seq.singleton firstPoint) secondPoint
        //let linearResult = resultLinear someSeq step

        //printfn "\nLinear:\n%A" linearResult

        let newPoint = readPoint ()
        let newSeqForLinear= seqCreate someSeq newPoint
        let newSeqForNewton= seqCreate someSeq newPoint
        doWhileRecursively step (Seq.tail newSeqForLinear) newSeqForNewton
    // | 1 -> 
    //     let pointsStr = File.ReadAllLines(args[0])
    //     let points = Seq.ofArray (Array.map (fun (str: string) -> ((str.Split(" ")[0] |> float), (str.Split(" ")[1] |> float))) (Array.tail pointsStr))
    //     let step = (Array.head pointsStr) |> float
    //     let linearResult = resultLinear points step
    //     let newtonResult = resultNewton points step
    //     printfn "Linear:\n"
    //     for pair in linearResult do
    //         printfn "%A" pair
    //     printfn "Newton:\n"
    //     for pair in newtonResult do
    //         printfn "%A" pair
    0
