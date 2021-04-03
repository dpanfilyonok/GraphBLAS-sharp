namespace GraphBLAS.FSharp.Algorithms

open GraphBLAS.FSharp.Predefined
open GraphBLAS.FSharp

module BC =
    let metric (matrix: Matrix<bool>) (source: int) = graphblas {
        let n = Matrix.rowCount matrix
        let! delta = Vector.zeroCreate<float> n
        let! sigma = Matrix.zeroCreate<int> n n
        let! q = Vector.ofList n [source, 1]
        let! p = Vector.copy q

        do! Vector.vxmWithMask AddMult.int (Vector.mask p) q matrix
        >>= Vector.assignVector q

        let mutable d = 0
        let mutable sum = 0
        let mutable isBreak = false

        while not isBreak || sum <> 0 do
            isBreak <- true

            do! Matrix.assignRow sigma d q

            do! Vector.eWiseAdd (Vector.EWiseAddAlgebra.ClosedBinaryOp <@ (+) @>) p q
            >>= Vector.assignVector p

            do! Vector.vxmWithMask (Vector.EWiseMultAlgebra.ClosedBinaryOp <@ (*) @>) (Vector.mask p) q matrix
            >>= Vector.assignVector q

            do! Vector.reduce Add.int q
            >>= fun (Scalar s) -> EvalGB.return' (sum <- s)

            d <- d + 1

        let! t1 = Vector.zeroCreate<float> n
        let! t2 = Vector.zeroCreate<float> n
        let! t3 = Vector.zeroCreate<float> n
        let! t4 = Vector.zeroCreate<float> n

        for i = d - 1 downto 1 do
            do! Vector.apply (UnaryOp <@ (+) 1. @>) delta
            >>= Vector.assignVector t1

            do!
                //Matrix.extractRow sigma i
                Matrix.extractRow i sigma
                >>= Vector.apply ^ UnaryOp <@ float @>
                >>= Vector.assignVector t2

            do! Vector.eWiseMultWithMask {
                    Algebra = ClosedBinaryOp <@ (/) @>
                    Pattern = q
                } t1 t2
            >>= Vector.assignVector t2

            do! Matrix.mxv AddMult.float matrix t2
            >>= Vector.assignVector t3

            do! Matrix.extractRow (i - 1) sigma
            >>= Vector.apply ^ UnaryOp <@ float @>
            >>= fun x -> Vector.eWiseMult AddMult.float x t3
            >>= Vector.assignVector t4

        return! Vector.eWiseAdd Add.float delta t4
    }

// с масками ужасно работается
// в ss есть setElement, который выполняется перед операцией а у нас???
// подумать про аккумулятор
// мб, чтобы все unit возвращали
// мб dsl
// мб как grblas
// мб интерпретатор
