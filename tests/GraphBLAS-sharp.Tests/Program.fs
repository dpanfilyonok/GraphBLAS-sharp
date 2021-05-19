open Expecto

open Brahma.OpenCL
open Brahma.FSharp.OpenCL.WorkflowBuilder.Basic
open Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation
open GraphBLAS.FSharp.Backend.Common
open Brahma.FSharp.OpenCL.Core
open OpenCL.Net
open GraphBLAS.FSharp
open GraphBLAS.FSharp.Algorithms
open GraphBLAS.FSharp.IO

[<Tests>]
let allTests =
    testList "All tests" [
        Backend.PrefixSum.tests
        Backend.BitonicSort.tests
        Backend.RemoveDuplicates.tests
        Matrix.EWiseAdd.tests
        Matrix.GetTuples.tests
        Matrix.Mxv.tests
        Matrix.Transpose.tests
        Algo.Bfs.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv

    // graphblas {
    //     let! matrix =
    //         MtxReader("webbase-1M.mtx").ReadMatrix(fun _ -> 1)
    //         |> Matrix.switch CSR
    //         >>= Matrix.synchronizeAndReturn
    //     let! vector = Vector.ofList matrix.ColumnCount (List.init matrix.ColumnCount (fun i -> i, 1))
    //     return!
    //         Matrix.mxv Predefined.AddMult.int matrix vector
    //         >>= Vector.synchronizeAndReturn
    // }
    // |> EvalGB.withClContext (OpenCLEvaluationContext())
    // |> EvalGB.runSync
    // |> printfn "%A"

    // 0
