namespace GraphBLAS.FSharp

open Brahma.FSharp.OpenCL.WorkflowBuilder.Basic

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =

    (*
        constructors
    *)

    let build (size: int) (indices: int[]) (values: 'a[]) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    let ofTuples (size: int) (tuples: VectorTuples<'a>) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    let ofList (size: int) (elements: (int * 'a) list) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    // можно оставить, но с условием, что будет создаваться full vector
    // let ofArray (array: 'a[]) : GraphblasEvaluation<Vector<'a>> =
    //     failwith "Not Implemented yet"

    let init (size: int) (initializer: int -> 'a) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    let create (size: int) (value: 'a) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    let zeroCreate<'a when 'a : struct> (size: int) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    (*
        methods
    *)

    let size (vector: Vector<'a>) : int = failwith "Not Implemented yet"
    let clear (vector: Vector<'a>) : GraphblasEvaluation<unit> = failwith "Not Implemented yet"
    let copy (vector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"
    let resize (size: int) (vector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"
    let nnz (vector: Vector<'a>) : GraphblasEvaluation<int> = failwith "Not Implemented yet"
    let tuples (vector: Vector<'a>) : GraphblasEvaluation<VectorTuples<'a>> = failwith "Not Implemented yet"
    let mask (vector: Vector<'a>) : Mask1D<_> = failwith "Not Implemented yet"
    let complemented (vector: Vector<'a>) : Mask1D<_> = failwith "Not Implemented yet"
    //let thin (isZero: 'a -> bool) (vector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"
    let switch (vectorType: VectorType) (vector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"
    let synchronize (vector: Vector<'a>) : GraphblasEvaluation<unit> = failwith "Not Implemented yet"

    (*
        assignment, extraction and filling
    *)

    /// vec.[mask]
    let extractSubVector (vector: Vector<'a>) (mask: Mask1D<_>) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    /// vec.[idx]
    let extractValue (vector: Vector<'a>) (idx: int) : GraphblasEvaluation<Scalar<'a>> =
        failwith "Not Implemented yet"

    /// t <- vec
    let assignVector (target: Vector<'a>) (source: Vector<'a>) : GraphblasEvaluation<unit> =
        failwith "Not Implemented yet"

    /// t.[mask] <- vec
    let assignSubVector (target: Vector<'a>) (mask: Mask1D<_>) (source: Vector<'a>) : GraphblasEvaluation<unit> =
        failwith "Not Implemented yet"

    /// t.[idx] <- value
    let assignValue (target: Vector<'a>) (idx: int) (value: Scalar<'a>) : GraphblasEvaluation<unit> =
        failwith "Not Implemented yet"

    /// vec.[*] <- value
    let fillVector (vector: Vector<'a>) (value: Scalar<'a>) : GraphblasEvaluation<unit> =
        failwith "Not Implemented yet"

    /// vec.[mask] <- value
    let fillSubVector (vector: Vector<'a>) (mask: Mask1D<_>) (value: Scalar<'a>) : GraphblasEvaluation<unit> =
        failwith "Not Implemented yet"

    (*
        operations
    *)

    type VxmContext<'a, 'b when 'b : struct> internal(algebra: VxmAlgebra<'a>, ?mask: Vector<'b>) =
        new(sr: Semiring<'a>, ?mask: Vector<'b>) = VxmContext(VxmSemiring sr)

        member internal this.Algebra = algebra
        member internal this.Mask = mask

    and internal VxmAlgebra<'a> =
        | VxmSemiring of Semiring<'a>

    let vxm (vector: Vector<'a>) (matrix: Matrix<'a>) (context: VxmContext<'a, _>) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    type EWiseAddContext<'a, 'b when 'b : struct> internal(algebra: EWiseAddAlgebra<'a>, ?mask: Vector<'b>) =
        new(plus: ClosedBinaryOp<'a>, ?mask: Vector<'b>) = EWiseAddContext(ClosedBinaryOpAlgebra plus)

        member internal this.Algebra = algebra
        member internal this.Mask = mask

    and EWiseAddAlgebra<'a> =
        | ClosedBinaryOpAlgebra of ClosedBinaryOp<'a>

    let eWiseAdd (leftVector: Vector<'a>) (rightVector: Vector<'a>) (context: EWiseAddContext<'a, _>) : GraphblasEvaluation<Vector<'a>> =
        match context.Algebra with
        | ClosedBinaryOpAlgebra (ClosedBinaryOp op) -> ()
        failwith "Not Implemented yet"

    type EWiseMultAlgebra<'a> =
        | ClosedBinaryOp of times: ClosedBinaryOp<'a>

    let eWiseMult (algebra: EWiseMultAlgebra<'a>) (leftVector: Vector<'a>) (rightVector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    type ApplyAlgebra<'a, 'b> =
        | UnaryOp of mapper: UnaryOp<'a, 'b>

    let apply (algebra: ApplyAlgebra<'a, 'b>) (vector: Vector<'a>) : GraphblasEvaluation<Vector<'b>> =
        failwith "Not Implemented yet"

    type SelectAlgebra<'a> =
        | UnaryOp of predicate: UnaryOp<'a, bool>

    let select (algebra: SelectAlgebra<'a>) (vector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> =
        failwith "Not Implemented yet"

    type ReduceAlgebra<'a> =
        | Monoid of plus: ClosedBinaryOp<'a> * zero: 'a

    let reduce (algebra: ReduceAlgebra<'a>) (vector: Vector<'a>) : GraphblasEvaluation<Scalar<'a>> =
        failwith "Not Implemented yet"

    // type VxmContext<'a> =
    //     {
    //         Algebra: VxmAlgebra<'a>
    //     }

    // type EWiseMultContext<'a> =
    //     {
    //         Algebra: EWiseMultAlgebra<'a>
    //     }

    // type ApplyContext<'a, 'b> =
    //     {
    //         Algebra: ApplyAlgebra<'a, 'b>
    //     }

    // type SelectContext<'a> =
    //     {
    //         Algebra: SelectAlgebra<'a>
    //     }

    // type ReduceContext<'a> =
    //     {
    //         Algebra: ReduceAlgebra<'a>
    //     }

    // let vxmWithMask (semiring: ISemiring<'a>) (mask: Mask1D<_>) (vector: Vector<'a>) (matrix: Matrix<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"
    // let eWiseAddWithMask (monoid: IMonoid<'a>) (mask: Mask1D<_>) (leftVector: Vector<'a>) (rightVector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"
    // let eWiseMultWithMask (semiring: ISemiring<'a>) (leftVector: Vector<'a>) (rightVector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"
    // let applyWithMask (mapper: UnaryOp<'a, 'b>) (mask: Mask1D<_>) (vector: Vector<'a>) : GraphblasEvaluation<Vector<'b>> = failwith "Not Implemented yet"
    // let selectWithMask (predicate: UnaryOp<'a, bool>) (mask: Mask1D<_>) (vector: Vector<'a>) : GraphblasEvaluation<Vector<'a>> = failwith "Not Implemented yet"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VectorTuples =
    let synchronize (vectorTuples: VectorTuples<'a>) =
        opencl {
            let! _ = ToHost vectorTuples.Indices
            let! _ = ToHost vectorTuples.Values
            return ()
        }
        |> EvalGB.fromCl
