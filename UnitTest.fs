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
    let actual = linearInterpolation (prepareXY x y) 0.00 1.00
    let expected = seq {(0.0, 0.0); (1.0, 0.6365372374); (2.0, 1.273074475)}
    Assert.True(compareResult expected actual)

[<Test>]
let Test2 () =
    let x =
        seq {
            yield 0.000
            yield 1
            yield 4
            yield 6
        }
    let y =
        seq {
            yield 0.000
            yield 2
            yield 5
            yield 9
        }

    let actual = newtonInterpolationFunc (Seq.zip x y) 5.00
    let expected = 6.5
    Assert.AreEqual(expected, actual)

[<Test>]
let Test3 () =
    let x =
        seq {
            yield 0.000
            yield 1
            yield 4
            yield 6
        }
    let y =
        seq {
            yield 0.000
            yield 2
            yield 5
            yield 9
        }

    let actual = newtonInterpolation (Seq.zip x y) 0.00 1.00
    let expected = seq {(0.0, 0.0); (1.0, 2); (2.0, 3.2); (3.0, 4.05); (4.0, 5.0); (5.0, 6.5); (6.0, 9.0)}
    Assert.True(compareResult expected actual)

[<Test>]
let UsingTest () =
    let firstPoint = (0.00, 0.00)
    let secondPoint = (1.571, 1.00)

    let seqCreate someSeq point = Seq.append someSeq (Seq.singleton point)
    let result someSeq = linearInterpolation (Seq.pairwise someSeq) (fst (Seq.head someSeq)) 1

    let firstSeq = seqCreate (Seq.singleton firstPoint) secondPoint
    let firstRes = result firstSeq

    let expected = seq {(0.0, 0.0); (1.0, 0.64); (2.0, 1.27)}
    Assert.True(compareResult expected firstRes)

    let oneMorePoint = (3.142, 0.00)
    let oneMoreSeq = seqCreate firstSeq oneMorePoint
    let oneMoreRes = result (Seq.tail oneMoreSeq)

    let expected = seq {(1.57, 1.00); (2.57, 0.36); (3.57, -0.27)}
    Assert.True(compareResult expected oneMoreRes)
    
    
