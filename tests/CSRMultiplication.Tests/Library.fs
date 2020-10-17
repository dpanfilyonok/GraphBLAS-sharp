namespace CSRMultiplication.Tests

open NUnit.Framework
open FsUnit
open CSRMultiplication
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

[<TestFixture>]
type MatrixMultiplicationTests () =
    let deviceType = DeviceType.Default
    let provider =
        try  ComputeProvider.Create("INTEL*", deviceType)
        with 
        | ex -> failwith ex.Message

    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)


    [<Test>]
    member this.``kekw`` () = 
        [
            [1;2;3]
            [1;2;3]
            [1;2;3]
        ]
        |> array2D
        |> Array2D.map float
        |> CSRMatrix.makeFromDenseMatrix
        |> SparseMatrixMultiplication.multiplySpMV provider commandQueue ([|1.;2.;3.|])
        |> Array.sum
        |> should (equalWithin 0.0001) 42.