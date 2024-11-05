module Program

open System
open Interpolation

let readPoint prompt =
    printfn "%s" prompt
    printf "X> "
    let x = Console.ReadLine() |> float
    printf "Y> "
    let y = Console.ReadLine() |> float
    (x, y)

let seqCreate someSeq point = Seq.append someSeq (Seq.singleton point)

let resultLinear someSeq step = 
    linearInterpolation (Seq.pairwise someSeq) (fst (Seq.head someSeq)) step

let resultNewton someSeq step = 
    newtonInterpolation someSeq (fst (Seq.head someSeq)) step

[<EntryPoint>]
let main _ = 
    printf "Step is: "
    let step = Console.ReadLine() |> float

    let firstPoint = readPoint "Enter the first point:"
    let secondPoint = readPoint "Enter the second point:"

    let mutable someSeq = seqCreate (Seq.singleton firstPoint) secondPoint
    let linearResult = resultLinear someSeq step

    printfn "\nLinear interpolation result:\n%A" linearResult

    while true do
        let newPoint = readPoint "Enter the next point:"
        someSeq <- seqCreate someSeq newPoint
        let linear = resultLinear someSeq step
        let newton = resultNewton someSeq step
        printfn "---"
        printfn "Linear interpolation:\n"
        for pair in linear do
            printfn "%A" pair
        printfn "Newton interpolation:\n"
        for pair in newton do
            printfn "%A" pair

    0
