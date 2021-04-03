namespace GraphBLAS.FSharp

[<AutoOpen>]
type AlgebraConstructor =
    static member Algebra(algebra: Vector.VxmAlgebra<'a>) : Vector.VxmContext<'a> = { Algebra = algebra }
    static member Algebra(algebra: Vector.EWiseAddAlgebra<'a>) : Vector.EWiseAddContext<'a> = { Algebra = algebra }
    static member Algebra(algebra: Vector.EWiseMultAlgebra<'a>) : Vector.EWiseMultContext<'a> = { Algebra = algebra }
    static member Algebra(algebra: Vector.ApplyAlgebra<'a, 'b>) : Vector.ApplyContext<'a, 'b> = { Algebra = algebra }
    static member Algebra(algebra: Vector.SelectAlgebra<'a>) : Vector.SelectContext<'a> = { Algebra = algebra }
    static member Algebra(algebra: Vector.ReduceAlgebra<'a>) : Vector.ReduceContext<'a> = { Algebra = algebra }
