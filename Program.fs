module Program

open System
open System.IO
open Interpolation

type ApproxState =
    { Name: string
      Points: (float * float) seq
      Step: float
      Func: (float * float) seq -> float -> float -> (float * float) seq }

let readPoint () =
    let point = Console.ReadLine().Split(" ")
    let x = point[0] |> float
    let y = point[1] |> float
    (x, y)

let seqCreate someSeq point =
    Seq.append someSeq (Seq.singleton point)

let culculateAndPrint approxName someSeq step approx =
    let result = approx someSeq (fst (Seq.head someSeq)) step
    printfn "%s\n" approxName

    for pair in result do
        printfn "%A" pair

let rec doWhileRecursively (states: seq<ApproxState>) =
    for state in states do
        culculateAndPrint state.Name state.Points state.Step state.Func

    let newPoint = readPoint ()

    let newStates =
        seq {
            for state in states do
                yield { state with Points = (seqCreate (Seq.tail state.Points) (newPoint)) }
        }

    doWhileRecursively newStates

[<EntryPoint>]
let main (args) =
    match Array.length args with
    | 0 ->
        printf "Step is: "
        let step = Console.ReadLine() |> float

        let firstPoint = readPoint ()
        let secondPoint = readPoint ()
        let someSeq = seqCreate (Seq.singleton firstPoint) secondPoint

        let result = linearInterpolation someSeq (fst (Seq.head someSeq)) step
        printfn "%s\n" "Linear"

        for pair in result do
            printfn "%A" pair

        let newPoint = readPoint ()

        let linear =
            { Name = "Linear"
              Points = Seq.tail (seqCreate someSeq newPoint)
              Step = step
              Func = linearInterpolation }

        let newton =
            { Name = "Newton"
              Points = seqCreate someSeq newPoint
              Step = step
              Func = newtonInterpolation }

        doWhileRecursively (Seq.append (Seq.singleton linear) (Seq.singleton newton))
    | 1 ->
        let pointsStr = File.ReadAllLines(args[0])

        let points =
            Seq.ofArray (
                Array.map
                    (fun (str: string) -> ((str.Split(" ")[0] |> float), (str.Split(" ")[1] |> float)))
                    (Array.tail pointsStr)
            )

        let step = (Array.head pointsStr) |> float

        culculateAndPrint "Linear" points step linearInterpolation
        culculateAndPrint "Newton" points step newtonInterpolation

    0
