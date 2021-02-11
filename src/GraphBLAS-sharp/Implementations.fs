namespace GraphBLAS.FSharp

open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open GlobalContext
open Helpers
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.WorkflowBuilder.Basic
open Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation
open Toolbox

type COOFormat<'a> = {
    Rows: int[]
    Columns: int[]
    Values: 'a[]
    RowCount: int
    ColumnCount: int
}

type CSRFormat<'a> = {
    Values: 'a[]
    Columns: int[]
    RowPointers: int[]
    ColumnCount: int
}
with
    static member CreateEmpty<'a>() = {
        Values = Array.zeroCreate<'a> 0
        Columns = Array.zeroCreate<int> 0
        RowPointers = Array.zeroCreate<int> 0
        ColumnCount = 0
    }

type CSRMatrix<'a when 'a : struct and 'a : equality>(csrTuples: CSRFormat<'a>) =
    inherit Matrix<'a>(csrTuples.RowPointers.Length - 1, csrTuples.ColumnCount)

    let rowCount = base.RowCount
    let columnCount = base.ColumnCount

    let spMV (vector: DenseVector<'a>) (mask: Mask1D) (semiring: Semiring<'a>) : OpenCLEvaluation<Vector<'a>> =
        let csrMatrixRowCount = rowCount
        let csrMatrixColumnCount = columnCount
        let vectorLength = vector.Size
        if csrMatrixColumnCount <> vectorLength then
            invalidArg
                "vector"
                (sprintf "Argument has invalid dimension. Need %i, but given %i" csrMatrixColumnCount vectorLength)

        let (BinaryOp plus) = semiring.PlusMonoid.Append
        let (BinaryOp mult) = semiring.Times

        let resultVector = Array.zeroCreate<'a> csrMatrixRowCount
        let command =
            <@
                fun (ndRange: _1D)
                    (resultBuffer: 'a[])
                    (csrValuesBuffer: 'a[])
                    (csrColumnsBuffer: int[])
                    (csrRowPointersBuffer: int[])
                    (vectorBuffer: 'a[]) ->

                    let i = ndRange.GlobalID0
                    let mutable localResultBuffer = resultBuffer.[i]
                    for k in csrRowPointersBuffer.[i] .. csrRowPointersBuffer.[i + 1] - 1 do
                        localResultBuffer <- (%plus) localResultBuffer
                            ((%mult) csrValuesBuffer.[k] vectorBuffer.[csrColumnsBuffer.[k]])
                    resultBuffer.[i] <- localResultBuffer
            @>

        let ndRange = _1D(csrMatrixRowCount)
        let binder = fun kernelPrepare ->
            kernelPrepare
                ndRange
                resultVector
                csrTuples.Values
                csrTuples.Columns
                csrTuples.RowPointers
                vector.Values

        opencl {
            do! RunCommand command binder
            return upcast DenseVector(resultVector, semiring.PlusMonoid)
        }

    // Not Implemented
    new(rows: int[], columns: int[], values: 'a[]) = CSRMatrix(CSRFormat.CreateEmpty())

    member this.Values = csrTuples.Values
    member this.Columns = csrTuples.Columns
    member this.RowPointers = csrTuples.RowPointers

    override this.Clear () = failwith "Not Implemented"
    override this.Copy () = failwith "Not Implemented"
    override this.Resize a b = failwith "Not Implemented"
    override this.GetNNZ () = failwith "Not Implemented"
    override this.GetTuples () = failwith "Not Implemented"
    override this.GetMask(?isComplemented: bool) =
        let isComplemented = defaultArg isComplemented false
        failwith "Not Implemented"
    override this.ToHost () = failwith "Not implemented"

    override this.Extract (mask: Mask2D option) : OpenCLEvaluation<Matrix<'a>> = failwith "Not Implemented"
    override this.Extract (colMask: Mask1D option * int) : OpenCLEvaluation<Vector<'a>> = failwith "Not Implemented"
    override this.Extract (rowMask: int * Mask1D option) : OpenCLEvaluation<Vector<'a>> = failwith "Not Implemented"
    override this.Extract (idx: int * int) : OpenCLEvaluation<Scalar<'a>> = failwith "Not Implemented"
    override this.Assign (mask: Mask2D option, value: Matrix<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (colMask: Mask1D option * int, value: Vector<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (rowMask: int * Mask1D option, value: Vector<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (idx: int * int, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (mask: Mask2D option, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (colMask: Mask1D option * int, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (rowMask: int * Mask1D option, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"

    override this.Mxm a b c = failwith "Not Implemented"
    override this.Mxv a b c = failwith "Not Implemented"
    override this.EWiseAdd a b c = failwith "Not Implemented"
    override this.EWiseMult a b c = failwith "Not Implemented"
    override this.Apply a b  = failwith "Not Implemented"
    override this.Prune a b = failwith "Not Implemented"
    override this.ReduceIn a b = failwith "Not Implemented"
    override this.ReduceOut a b = failwith "Not Implemented"
    override this.Reduce a = failwith "Not Implemented"
    override this.Transpose () = failwith "Not Implemented"
    override this.Kronecker a b c = failwith "Not Implemented"

and COOMatrix<'a when 'a : struct and 'a : equality>(rowCount: int, columnCount: int, rows: int[], columns: int[], values: 'a[]) =
    inherit Matrix<'a>(rowCount, columnCount)

    let mutable rows, columns, values = rows, columns, values
    member this.Rows with get() = rows
    member this.Columns with get() = columns
    member this.Values with get() = values
    member this.Elements with get() = (rows, columns, values) |||> Array.zip3

    override this.Clear () = failwith "Not Implemented"
    override this.Copy () = failwith "Not Implemented"
    override this.Resize a b = failwith "Not Implemented"
    override this.GetNNZ () = failwith "Not Implemented"

    override this.GetTuples () =
        opencl {
            return {| Rows = this.Rows; Columns = this.Columns; Values = this.Values |}
        }

    override this.GetMask(?isComplemented: bool) =
        let isComplemented = defaultArg isComplemented false
        failwith "Not Implemented"

    override this.ToHost () =
        opencl {
            let! _ = ToHost this.Rows
            let! _ = ToHost this.Columns
            let! _ = ToHost this.Values

            return upcast this
        }

    override this.Extract (mask: Mask2D option) : OpenCLEvaluation<Matrix<'a>> = failwith "Not Implemented"
    override this.Extract (colMask: Mask1D option * int) : OpenCLEvaluation<Vector<'a>> = failwith "Not Implemented"
    override this.Extract (rowMask: int * Mask1D option) : OpenCLEvaluation<Vector<'a>> = failwith "Not Implemented"
    override this.Extract (idx: int * int) : OpenCLEvaluation<Scalar<'a>> = failwith "Not Implemented"
    override this.Assign (mask: Mask2D option, value: Matrix<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (colMask: Mask1D option * int, value: Vector<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (rowMask: int * Mask1D option, value: Vector<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (idx: int * int, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (mask: Mask2D option, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (colMask: Mask1D option * int, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (rowMask: int * Mask1D option, value: Scalar<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"

    override this.Mxm a b c = failwith "Not Implemented"
    override this.Mxv a b c = failwith "Not Implemented"

    member internal this.EWiseAddCOO
        (matrix: COOMatrix<'a>)
        (mask: Mask2D option)
        (semiring: Semiring<'a>) : OpenCLEvaluation<Matrix<'a>> =

        let (BinaryOp append) = semiring.PlusMonoid.Append
        let zero = semiring.PlusMonoid.Zero

        //It is useful to consider that the first array is longer than the second one
        let firstRows, firstColumns, firstValues, secondRows, secondColumns, secondValues, plus =
            if this.Rows.Length > matrix.Rows.Length then
                this.Rows, this.Columns, this.Values, matrix.Rows, matrix.Columns, matrix.Values, append
            else
                matrix.Rows, matrix.Columns, matrix.Values, this.Rows, this.Columns, this.Values, <@ fun x y -> (%append) y x @>

        let filterThroughMask =
            opencl {
                //TODO
                ()
            }

        let allRows = Array.zeroCreate <| firstRows.Length + secondRows.Length
        let allColumns = Array.zeroCreate <| firstColumns.Length + secondColumns.Length
        let allValues = Array.init (firstValues.Length + secondValues.Length) (fun _ -> zero)

        let longSide = firstRows.Length
        let shortSide = secondRows.Length

        let allRowsLength = allRows.Length

        // let createSortedConcatenation =
        //     <@
        //         fun (ndRange: _1D)
        //             (firstRowsBuffer: int[])
        //             (firstColumnsBuffer: int[])
        //             (firstValuesBuffer: 'a[])
        //             (secondRowsBuffer: int[])
        //             (secondColumnsBuffer: int[])
        //             (secondValuesBuffer: 'a[])
        //             (allRowsBuffer: int[])
        //             (allColumnsBuffer: int[])
        //             (allValuesBuffer: 'a[]) ->

        //             let i = ndRange.GlobalID0

        //             if i < allRowsLength then
        //                 let f n = if 0 > n + 1 - shortSide then 0 else n + 1 - shortSide
        //                 let mutable leftEdge = f i
        //                     // if 0 > i + 1 - shortSide then 0 else i + 1 - shortSide

        //                 let g n = if n > longSide - 1 then longSide - 1 else n
        //                 let mutable rightEdge = g i
        //                     // if i > longSide - 1 then longSide - 1 else i

        //                 while leftEdge <= rightEdge do
        //                     let middleIdx = (leftEdge + rightEdge) / 2
        //                     let firstRow = firstRowsBuffer.[middleIdx]
        //                     let firstColumn = firstColumnsBuffer.[middleIdx]
        //                     let secondRow = secondRowsBuffer.[i - middleIdx]
        //                     let secondColumn = secondColumnsBuffer.[i - middleIdx]
        //                     if firstRow < secondRow || firstRow = secondRow && firstColumn < secondColumn then leftEdge <- middleIdx + 1 else rightEdge <- middleIdx - 1

        //                 let boundaryX = rightEdge
        //                 let boundaryY = i - leftEdge
        //                 let firstRow = firstRowsBuffer.[boundaryX]
        //                 let firstColumn = firstColumnsBuffer.[boundaryX]
        //                 let secondRow = secondRowsBuffer.[boundaryY]
        //                 let secondColumn = secondColumnsBuffer.[boundaryY]

        //                 if boundaryX < 0 || boundaryY >= 0 && (firstRow < secondRow || firstRow = secondRow && firstColumn < secondColumn) then
        //                     allRowsBuffer.[i] <- secondRowsBuffer.[boundaryY]
        //                     allColumnsBuffer.[i] <- secondColumnsBuffer.[boundaryY]
        //                     allValuesBuffer.[i] <- secondValuesBuffer.[boundaryY]
        //                 else
        //                     allRowsBuffer.[i] <- firstRowsBuffer.[boundaryX]
        //                     allColumnsBuffer.[i] <- firstColumnsBuffer.[boundaryX]
        //                     allValuesBuffer.[i] <- firstValuesBuffer.[boundaryX]
        //     @>

        let knots = Array.zeroCreate <| (shortSide + longSide + workGroupSize - 1) / workGroupSize + 1
        knots.[knots.Length - 1] <- longSide
        let knotsLength = knots.Length

        let workGroupSize = Toolbox.workGroupSize

        let prepareToCreateSortedConcatenation =
            <@
                fun (ndRange: _1D)
                    (firstRowsBuffer: int[])
                    (firstColumnsBuffer: int[])
                    (secondRowsBuffer: int[])
                    (secondColumnsBuffer: int[])
                    (knotsBuffer: int[]) ->

                    let i = ndRange.GlobalID0 + 1

                    if i < knotsLength then
                        let mutable x = i * workGroupSize - 1
                        if x >= shortSide + longSide then x <- shortSide + longSide - 1
                        let diagonalNumber = x

                        let mutable leftEdge = 0
                        if 0 <= diagonalNumber + 1 - shortSide then leftEdge <- diagonalNumber + 1 - shortSide

                        let mutable rightEdge = diagonalNumber
                        if diagonalNumber > longSide - 1 then rightEdge <- longSide - 1

                        while leftEdge <= rightEdge do
                            let middleIdx = (leftEdge + rightEdge) / 2
                            let firstRow = firstRowsBuffer.[middleIdx]
                            let firstColumn = firstColumnsBuffer.[middleIdx]
                            let secondRow = secondRowsBuffer.[diagonalNumber - middleIdx]
                            let secondColumn = secondColumnsBuffer.[diagonalNumber - middleIdx]
                            if firstRow < secondRow || firstRow = secondRow && firstColumn < secondColumn then leftEdge <- middleIdx + 1 else rightEdge <- middleIdx - 1

                        knotsBuffer.[i] <- leftEdge
            @>

        let prepareToCreateSortedConcatenation =
            opencl {
                let binder kernelP =
                    let ndRange = _1D(workSize (knotsLength - 1), workGroupSize)
                    kernelP
                        ndRange
                        firstRows
                        firstColumns
                        secondRows
                        secondColumns
                        knots
                do! RunCommand prepareToCreateSortedConcatenation binder
            }

        let createSortedConcatenation =
            <@
                fun (ndRange: _1D)
                    (knotsBuffer: int[])
                    (firstRowsBuffer: int[])
                    (firstColumnsBuffer: int[])
                    (firstValuesBuffer: 'a[])
                    (secondRowsBuffer: int[])
                    (secondColumnsBuffer: int[])
                    (secondValuesBuffer: 'a[])
                    (allRowsBuffer: int[])
                    (allColumnsBuffer: int[])
                    (allValuesBuffer: 'a[]) ->

                    let i = ndRange.GlobalID0

                    if i < allRowsLength then
                        let localID = ndRange.LocalID0
                        let workGroupNumber = i / workGroupSize
                        let beginIdx = knotsBuffer.[workGroupNumber]
                        let endIdx = knotsBuffer.[workGroupNumber + 1]
                        let firstLocalLength = endIdx - beginIdx
                        let mutable x = workGroupSize - firstLocalLength
                        if endIdx = longSide then x <- shortSide - i + localID + beginIdx
                        let secondLocalLength = x

                        let firstRowsLocalBuffer = localArray workGroupSize
                        let firstColumnsLocalBuffer = localArray workGroupSize
                        let secondRowsLocalBuffer = localArray workGroupSize
                        let secondColumnsLocalBuffer = localArray workGroupSize

                        if localID < firstLocalLength then
                            firstRowsLocalBuffer.[localID] <- firstRowsBuffer.[beginIdx + localID]
                            firstColumnsLocalBuffer.[localID] <- firstColumnsBuffer.[beginIdx + localID]
                        if localID < secondLocalLength then
                            secondRowsLocalBuffer.[localID] <- secondRowsBuffer.[i - beginIdx]
                            secondColumnsLocalBuffer.[localID] <- secondColumnsBuffer.[i - beginIdx]

                        barrier ()

                        let mutable leftEdge = 0
                        if 0 <= localID + 1 - secondLocalLength then leftEdge <- localID + 1 - secondLocalLength

                        let mutable rightEdge = localID
                        if localID > firstLocalLength - 1 then rightEdge <- firstLocalLength - 1

                        while leftEdge <= rightEdge do
                            let middleIdx = (leftEdge + rightEdge) / 2
                            let firstRow = firstRowsLocalBuffer.[middleIdx]
                            let firstColumn = firstColumnsLocalBuffer.[middleIdx]
                            let secondRow = secondRowsLocalBuffer.[localID - middleIdx]
                            let secondColumn = secondColumnsLocalBuffer.[localID - middleIdx]
                            if firstRow < secondRow || firstRow = secondRow && firstColumn < secondColumn then leftEdge <- middleIdx + 1 else rightEdge <- middleIdx - 1

                        let boundaryX = rightEdge
                        let boundaryY = localID - leftEdge
                        let firstRow = firstRowsLocalBuffer.[boundaryX]
                        let firstColumn = firstColumnsLocalBuffer.[boundaryX]
                        let secondRow = secondRowsLocalBuffer.[boundaryY]
                        let secondColumn = secondColumnsLocalBuffer.[boundaryY]

                        if boundaryX < 0 || boundaryY >= 0 && (firstRow < secondRow || firstRow = secondRow && firstColumn < secondColumn) then
                            allRowsBuffer.[i] <- secondRowsLocalBuffer.[boundaryY]
                            allColumnsBuffer.[i] <- secondColumnsLocalBuffer.[boundaryY]
                            allValuesBuffer.[i] <- secondValuesBuffer.[i - localID - beginIdx + boundaryY]
                        else
                            allRowsBuffer.[i] <- firstRowsLocalBuffer.[boundaryX]
                            allColumnsBuffer.[i] <- firstColumnsLocalBuffer.[boundaryX]
                            allValuesBuffer.[i] <- firstValuesBuffer.[beginIdx + boundaryX]
            @>

        let createSortedConcatenation =
            opencl {
                let binder kernelP =
                    let ndRange = _1D(workSize allRows.Length, workGroupSize)
                    kernelP
                        ndRange
                        knots
                        firstRows
                        firstColumns
                        firstValues
                        secondRows
                        secondColumns
                        secondValues
                        allRows
                        allColumns
                        allValues
                do! RunCommand createSortedConcatenation binder
            }

        let auxiliaryArray = Array.init allRows.Length (fun _ -> 1)

        let fillAuxiliaryArray =
            <@
                fun (ndRange: _1D)
                    (allRowsBuffer: int[])
                    (allColumnsBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (auxiliaryArrayBuffer: int[]) ->

                    let i = ndRange.GlobalID0 + 1

                    if i < allRowsLength && allRowsBuffer.[i - 1] = allRowsBuffer.[i] && allColumnsBuffer.[i - 1] = allColumnsBuffer.[i] then
                        auxiliaryArrayBuffer.[i] <- 0
                        let localResultBuffer = (%plus) allValuesBuffer.[i - 1] allValuesBuffer.[i]
                        //if localResultBuffer = zero then auxiliaryArrayBuffer.[i] <- 0 else allValuesBuffer.[i] <- localResultBuffer
                        allValuesBuffer.[i - 1] <- localResultBuffer
            @>

        let fillAuxiliaryArray =
            opencl {
                let binder kernelP =
                    let ndRange = _1D(workSize (allRows.Length - 1), workGroupSize)
                    kernelP
                        ndRange
                        allRows
                        allColumns
                        allValues
                        auxiliaryArray
                do! RunCommand fillAuxiliaryArray binder
            }

        let auxiliaryArrayLength = auxiliaryArray.Length

        let createUnion =
            <@
                fun (ndRange: _1D)
                    (allRowsBuffer: int[])
                    (allColumnsBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (auxiliaryArrayBuffer: int[])
                    (prefixSumArrayBuffer: int[])
                    (resultRowsBuffer: int[])
                    (resultColumnsBuffer: int[])
                    (resultValuesBuffer: 'a[]) ->

                    let i = ndRange.GlobalID0

                    if i < auxiliaryArrayLength && auxiliaryArrayBuffer.[i] = 1 then
                        let index = prefixSumArrayBuffer.[i] - 1

                        resultRowsBuffer.[index] <- allRowsBuffer.[i]
                        resultColumnsBuffer.[index] <- allColumnsBuffer.[i]
                        resultValuesBuffer.[index] <- allValuesBuffer.[i]
            @>

        let resultRows = Array.zeroCreate allRows.Length
        let resultColumns = Array.zeroCreate allColumns.Length
        let resultValues = Array.init allValues.Length (fun _ -> zero)

        let createUnion =
            opencl {
                let! prefixSumArray = Toolbox.prefixSum auxiliaryArray
                let binder kernelP =
                    let ndRange = _1D(workSize auxiliaryArray.Length, workGroupSize)
                    kernelP
                        ndRange
                        allRows
                        allColumns
                        allValues
                        auxiliaryArray
                        prefixSumArray
                        resultRows
                        resultColumns
                        resultValues
                do! RunCommand createUnion binder
            }

        opencl {
            do! prepareToCreateSortedConcatenation
            do! createSortedConcatenation
            do! filterThroughMask
            do! fillAuxiliaryArray
            //do! Toolbox.EWiseAdd.dropExplicitZeroes zero allValues auxiliaryArray
            do! createUnion

            return upcast COOMatrix<'a>(this.RowCount, this.ColumnCount, resultRows, resultColumns, resultValues)
        }

    override this.EWiseAdd
        (matrix: Matrix<'a>)
        (mask: Mask2D option)
        (semiring: Semiring<'a>) =

        if (this.RowCount, this.ColumnCount) <> (matrix.RowCount, matrix.ColumnCount) then
            invalidArg
                "matrix"
                (sprintf "Argument has invalid dimension. Need %A, but given %A" (this.RowCount, this.ColumnCount) (matrix.RowCount, matrix.ColumnCount))

        // let mask =
        //     match matrixMask with
        //     | Some m ->
        //         if (m.RowCount, m.ColumnCount) <> (this.RowCount, this.ColumnCount) then
        //             invalidArg
        //                 "mask"
        //                 (sprintf "Argument has invalid dimension. Need %A, but given %A" (this.RowCount, this.ColumnCount) (m.RowCount, m.ColumnCount))
        //         m
        //     | _ -> Mask2D(Array.empty, this.RowCount, this.ColumnCount, true) // Empty complemented mask is equal to none

        match matrix with
        | :? COOMatrix<'a> -> this.EWiseAddCOO (downcast matrix) mask semiring
        | _ -> failwith "Not Implemented"

    override this.EWiseMult a b c = failwith "Not Implemented"
    override this.Apply a b  = failwith "Not Implemented"
    override this.Prune a b = failwith "Not Implemented"
    override this.ReduceIn a b = failwith "Not Implemented"
    override this.ReduceOut a b = failwith "Not Implemented"
    override this.Reduce a = failwith "Not Implemented"
    override this.Transpose () = failwith "Not Implemented"
    override this.Kronecker a b c = failwith "Not Implemented"

and SparseVector<'a when 'a : struct and 'a : equality>(size: int, indices: int[], values: 'a[]) =
    inherit Vector<'a>(size)

    let mutable indices, values = indices, values
    member this.Values with get() = values
    member this.Indices with get() = indices
    member this.Elements with get() = (indices, values) ||> Array.zip

    override this.Clear () = failwith "Not Implemented"
    override this.Copy () = failwith "Not Implemented"
    override this.Resize a = failwith "Not Implemented"
    override this.GetNNZ () = failwith "Not Implemented"

    override this.GetTuples () =
        opencl {
            return {| Indices = this.Indices; Values = this.Values |}
        }

    override this.GetMask(?isComplemented: bool) =
        let isComplemented = defaultArg isComplemented false
        failwith "Not Implemented"

    override this.ToHost () =
        opencl {
            let! _ = ToHost this.Indices
            let! _ = ToHost this.Values

            return upcast this
        }

    override this.Extract (mask: Mask1D option) : OpenCLEvaluation<Vector<'a>> = failwith "Not Implemented"
    override this.Extract (idx: int) : OpenCLEvaluation<Scalar<'a>> = failwith "Not Implemented"
    override this.Assign (mask: Mask1D option, vector: Vector<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (idx: int, Scalar (value: 'a)) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (mask: Mask1D option, Scalar (value: 'a)) : OpenCLEvaluation<unit> = failwith "Not Implemented"

    override this.Vxm (matrix: Matrix<'a>) (mask: Mask1D option) (semiring: Semiring<'a>) : OpenCLEvaluation<Vector<'a>> = failwith "Not Implemented"

    member internal this.EWiseAddSparse
        (vector: SparseVector<'a>)
        (mask: Mask1D option)
        (semiring: Semiring<'a>) : OpenCLEvaluation<Vector<'a>> =

        let (BinaryOp append) = semiring.PlusMonoid.Append
        let zero = semiring.PlusMonoid.Zero

        //It is useful to consider that the first array is longer than the second one
        let firstIndices, firstValues, secondIndices, secondValues, plus =
            if this.Indices.Length > vector.Indices.Length then
                this.Indices, this.Values, vector.Indices, vector.Values, append
            else
                vector.Indices, vector.Values, this.Indices, this.Values, <@ fun x y -> (%append) y x @>

        let filterThroughMask =
            opencl {
                //TODO
                ()
            }

        let allIndices = Array.zeroCreate <| firstIndices.Length + secondIndices.Length
        let allValues = Array.init (firstValues.Length + secondValues.Length) (fun _ -> zero)

        let longSide = firstIndices.Length
        let shortSide = secondIndices.Length

        let allIndicesLength = allIndices.Length

        let createSortedConcatenation =
            <@
                fun (ndRange: _1D)
                    (firstIndicesBuffer: int[])
                    (firstValuesBuffer: 'a[])
                    (secondIndicesBuffer: int[])
                    (secondValuesBuffer: 'a[])
                    (allIndicesBuffer: int[])
                    (allValuesBuffer: 'a[]) ->

                    let i = ndRange.GlobalID0
                    if i < allIndicesLength then
                        let f n = if 0 > n + 1 - shortSide then 0 else n + 1 - shortSide
                        let mutable leftEdge = f i
                            // if 0 > i + 1 - shortSide then 0 else i + 1 - shortSide

                        let g n = if n > longSide - 1 then longSide - 1 else n
                        let mutable rightEdge = g i
                            // if i > longSide - 1 then longSide - 1 else i

                        while leftEdge <= rightEdge do
                            let middleIdx = (leftEdge + rightEdge) / 2
                            if firstIndicesBuffer.[middleIdx] < secondIndicesBuffer.[i - middleIdx] then leftEdge <- middleIdx + 1 else rightEdge <- middleIdx - 1

                        let boundaryX, boundaryY = rightEdge, i - leftEdge

                        if boundaryX < 0 || boundaryY >= 0 && firstIndicesBuffer.[boundaryX] < secondIndicesBuffer.[boundaryY] then
                            allIndicesBuffer.[i] <- secondIndicesBuffer.[boundaryY]
                            allValuesBuffer.[i] <- secondValuesBuffer.[boundaryY]
                        else
                            allIndicesBuffer.[i] <- firstIndicesBuffer.[boundaryX]
                            allValuesBuffer.[i] <- firstValuesBuffer.[boundaryX]
            @>

        let createSortedConcatenation =
            opencl {
                let binder kernelP =
                    let ndRange = _1D(workSize allIndices.Length, workGroupSize)
                    kernelP
                        ndRange
                        firstIndices
                        firstValues
                        secondIndices
                        secondValues
                        allIndices
                        allValues
                do! RunCommand createSortedConcatenation binder
            }

        let auxiliaryArray = Array.init allIndices.Length (fun _ -> 1)

        let fillAuxiliaryArray =
            <@
                fun (ndRange: _1D)
                    (allIndicesBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (auxiliaryArrayBuffer: int[]) ->

                    let i = ndRange.GlobalID0

                    if i + 1 < allIndicesLength && allIndicesBuffer.[i] = allIndicesBuffer.[i + 1] then
                        auxiliaryArrayBuffer.[i + 1] <- 0
                        let localResultBuffer = (%plus) allValuesBuffer.[i] allValuesBuffer.[i + 1]
                        if localResultBuffer = zero then auxiliaryArrayBuffer.[i] <- 0 else allValuesBuffer.[i] <- localResultBuffer
            @>

        let fillAuxiliaryArray =
            opencl {
                let binder kernelP =
                    let ndRange = _1D(workSize (allIndices.Length - 1), workGroupSize)
                    kernelP
                        ndRange
                        allIndices
                        allValues
                        auxiliaryArray
                do! RunCommand fillAuxiliaryArray binder
            }

        let auxiliaryArrayLength = auxiliaryArray.Length

        let createUnion =
            <@
                fun (ndRange: _1D)
                    (allIndicesBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (auxiliaryArrayBuffer: int[])
                    (prefixSumArrayBuffer: int[])
                    (resultIndicesBuffer: int[])
                    (resultValuesBuffer: 'a[]) ->

                    let i = ndRange.GlobalID0

                    if i < auxiliaryArrayLength && auxiliaryArrayBuffer.[i] = 1 then
                        let index = prefixSumArrayBuffer.[i] - 1

                        resultIndicesBuffer.[index] <- allIndicesBuffer.[i]
                        resultValuesBuffer.[index] <- allValuesBuffer.[i]
            @>

        let resultIndices = Array.zeroCreate allIndices.Length
        let resultValues = Array.init allValues.Length (fun _ -> zero)

        let createUnion =
            opencl {
                let! prefixSumArray = Toolbox.prefixSum auxiliaryArray
                let binder kernelP =
                    let ndRange = _1D(workSize auxiliaryArray.Length, workGroupSize)
                    kernelP
                        ndRange
                        allIndices
                        allValues
                        auxiliaryArray
                        prefixSumArray
                        resultIndices
                        resultValues
                do! RunCommand createUnion binder
            }

        opencl {
            do! createSortedConcatenation
            do! filterThroughMask
            do! fillAuxiliaryArray
            //do! Toolbox.EWiseAdd.dropExplicitZeroes zero allValues auxiliaryArray
            do! createUnion

            return upcast SparseVector<'a>(this.Size, resultIndices, resultValues)
        }

    override this.EWiseAdd
        (vector: Vector<'a>)
        (mask: Mask1D option)
        (semiring: Semiring<'a>) =

        if vector.Size <> this.Size then
            invalidArg
                "vector"
                (sprintf "Argument has invalid dimension. Need %i, but given %i" this.Size vector.Size)

        // let mask =
        //     match mask with
        //     | Some m ->
        //         if m.Size <> this.Size then
        //             invalidArg
        //                 "mask"
        //                 (sprintf "Argument has invalid dimension. Need %i, but given %i" this.Size m.Size)
        //         m
        //     | _ -> Mask1D(Array.empty, this.Size, true) // Empty complemented mask is equal to none

        match vector with
        | :? SparseVector<'a> -> this.EWiseAddSparse (downcast vector) mask semiring
        | _ -> failwith "Not Implemented"

    override this.EWiseMult a b c = failwith "Not Implemented"
    override this.Apply a b = failwith "Not Implemented"
    override this.Prune a b = failwith "Not Implemented"
    override this.Reduce (monoid: Monoid<'a>) = failwith "Not Implemented"

and DenseVector<'a when 'a : struct and 'a : equality>(vector: 'a[], monoid: Monoid<'a>) =
    inherit Vector<'a>(vector.Length)

    member this.Monoid = monoid
    member this.Values: 'a[] = vector

    override this.Clear () = failwith "Not Implemented"
    override this.Copy () = failwith "Not Implemented"
    override this.Resize a = failwith "Not Implemented"
    override this.GetNNZ () = failwith "Not Implemented"
    override this.GetTuples () = failwith "Not Implemented"
    override this.GetMask(?isComplemented: bool) =
        let isComplemented = defaultArg isComplemented false
        failwith "Not Implemented"
    override this.ToHost () = failwith "Not Implemented"

    override this.Extract (mask: Mask1D option) : OpenCLEvaluation<Vector<'a>> = failwith "Not Implemented"
    override this.Extract (idx: int) : OpenCLEvaluation<Scalar<'a>> = failwith "Not Implemented"
    override this.Assign (mask: Mask1D option, vector: Vector<'a>) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (idx: int, Scalar (value: 'a)) : OpenCLEvaluation<unit> = failwith "Not Implemented"
    override this.Assign (mask: Mask1D option, Scalar (value: 'a)) : OpenCLEvaluation<unit> = failwith "Not Implemented"

    override this.Vxm a b c = failwith "Not Implemented"
    override this.EWiseAdd a b c = failwith "Not Implemented"
    override this.EWiseMult a b c = failwith "Not Implemented"
    override this.Apply a b = failwith "Not Implemented"
    override this.Prune a b = failwith "Not Implemented"
    override this.Reduce (monoid: Monoid<'a>) = failwith "Not Implemented"
