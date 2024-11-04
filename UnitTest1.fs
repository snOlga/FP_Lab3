module FP_Lab3

open NUnit.Framework
open Interpolation

let rec compareResult part1 part2 =
    Seq.forall2 (fun pair1 pair2 -> 
        ((snd pair1) + 0.01 > (snd pair2) && (snd pair1) - 0.01 < (snd pair2))) 
        part1 part2 

[<Test>]
let Test1 () =
    let x =
        seq {
            yield 0.000
            yield 1.571
        }
    let y =
        seq {
            yield 0.000
            yield 1
        }
    let actual = linearInterpolationWithPairs (prepareXY x y) 0.00 1.00 Seq.empty
    let expected = seq {(0.0, 0.0); (1.0, 0.6365372374); (2.0, 1.273074475)}
    Assert.True(compareResult expected actual)
