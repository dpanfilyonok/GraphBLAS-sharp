namespace GraphBLAS.FSharp.Algorithms

open GraphBLAS.FSharp.Predefined
open GraphBLAS.FSharp

module BFS =
    let levelSingleSource (matrix: Matrix<bool>) (source: int) = graphblas {
        let vertexCount = Matrix.rowCount matrix
        let! levels = Vector.zeroCreate vertexCount
        let! frontier = Vector.ofList vertexCount [source, true]

        let mutable currentLevel = 1
        while currentLevel < vertexCount do
            do!
                levels
                |> Vector.fillSubVector (Mask1D frontier) (Scalar currentLevel)

            let! frontier =
                (frontier, matrix)
                ||> Vector.vxmWithMask AnyAll.bool (Complemented1D levels)

            currentLevel <- currentLevel + 1

        return levels
    }
