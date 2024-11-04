module Interpolation

let linearInterpolation (x0: float) (y0: float) (x1: float) (y1: float) (x: float) =
    y0 * (x1 - x) / (x1 - x0) + y1 * (x - x0) / (x1 - x0)

let prepareXY (x: seq<float>) (y: seq<float>) =
    let xy = Seq.zip x y
    Seq.pairwise xy

let rec linearInterpolationWithPairs xyPairs (currentStep: float) (incStep: float) result=
    match Seq.length xyPairs with
    | 0 -> Seq.empty
    | _ -> 
        let newY = linearInterpolation
                    (fst (fst (Seq.head xyPairs)))
                    (snd (fst (Seq.head xyPairs)))
                    (fst (snd (Seq.head xyPairs)))
                    (snd (snd (Seq.head xyPairs)))
                    currentStep
        let resultSeq = seq{ yield (currentStep, newY)}

        if currentStep >= (fst (fst (Seq.head xyPairs))) && currentStep <= (fst (snd (Seq.head xyPairs))) then
            Seq.append resultSeq (linearInterpolationWithPairs xyPairs (currentStep + incStep) incStep result)
        else
            Seq.append resultSeq (linearInterpolationWithPairs (Seq.tail xyPairs) (currentStep + incStep) incStep result)
