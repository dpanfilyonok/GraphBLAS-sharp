namespace GraphBLAS.FSharp

type MaskType =
    | Regular
    | Complemented
    | NoMask

type Mask1D<'a when 'a : struct> =
    | Mask1D of Vector<'a>
    | Complemented1D of Vector<'a>

type Mask2D<'a when 'a : struct> =
    | Mask2D of Matrix<'a>
    | Complemented2D of Matrix<'a>
