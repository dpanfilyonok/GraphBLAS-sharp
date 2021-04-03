namespace GraphBLAS.FSharp.Backend.COOVector

open Brahma.FSharp.OpenCL.WorkflowBuilder.Basic
open Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation
open GraphBLAS.FSharp
open GraphBLAS.FSharp.Backend.Common
open GraphBLAS.FSharp.Backend.COOVector.Utilities

module internal EWiseAdd =
    let private runNonEmpty (leftIndices: int[]) (leftValues: 'a[]) (rightIndices: int[]) (rightValues: 'a[]) (mask: Mask1D<_> option) (semiring: ISemiring<'a>) : OpenCLEvaluation<int[] * 'a[]> = opencl {
        let! allIndices, allValues = merge leftIndices leftValues rightIndices rightValues mask

        let (ClosedBinaryOp plus) = semiring.Plus
        let! rawPositions = preparePositions allIndices allValues plus

        return! setPositions allIndices allValues rawPositions
    }

    let run (leftIndices: int[]) (leftValues: 'a[]) (rightIndices: int[]) (rightValues: 'a[]) (mask: Mask1D<_> option) (semiring: ISemiring<'a>) : OpenCLEvaluation<int[] * 'a[]> =
        if leftValues.Length = 0 then
            opencl {
                let! resultIndices = Copy.run rightIndices
                let! resultValues = Copy.run rightValues

                return resultIndices, resultValues
            }

        elif rightIndices.Length = 0 then
            opencl {
                let! resultIndices = Copy.run leftIndices
                let! resultValues = Copy.run leftValues

                return resultIndices, resultValues
            }

        else
            runNonEmpty leftIndices leftValues rightIndices rightValues mask semiring
