module Program

open System
open Interpolation

let readPoint () =
    let point = Console.ReadLine().Split(" ")
    let x = point[0] |> float
    let y = point[1] |> float
    (x, y)

let seqCreate someSeq point = Seq.append someSeq (Seq.singleton point)

let resultLinear someSeq step = 
    linearInterpolation (Seq.pairwise someSeq) (fst (Seq.head someSeq)) step

let resultNewton someSeq step = 
    newtonInterpolation someSeq (fst (Seq.head someSeq)) step

let rec doWhileRecursively step currentPointsForLinear currentPointsForNewton =
    let linear = resultLinear currentPointsForLinear step
    let newton = resultNewton currentPointsForNewton step
    printfn "Linear:\n"
    for pair in linear do
        printfn "%A" pair
    printfn "Newton:\n"
    for pair in newton do
        printfn "%A" pair
    let newPoint = readPoint ()
    let newSeqForLinear= seqCreate currentPointsForLinear newPoint
    let newSeqForNewton= seqCreate currentPointsForNewton newPoint
    doWhileRecursively step (Seq.tail newSeqForLinear) (Seq.tail newSeqForNewton)

[<EntryPoint>]
let main _ = 
    printf "Step is: "
    let step = Console.ReadLine() |> float

    let firstPoint = readPoint ()
    let secondPoint = readPoint ()

    let someSeq = seqCreate (Seq.singleton firstPoint) secondPoint
    let linearResult = resultLinear someSeq step

    printfn "\nLinear:\n%A" linearResult

    let newPoint = readPoint ()
    let newSeqForLinear= seqCreate someSeq newPoint
    let newSeqForNewton= seqCreate someSeq newPoint
    doWhileRecursively step (Seq.tail newSeqForLinear) newSeqForNewton
    0
