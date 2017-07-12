-- MyComplex.dsl
-- define a new type called "Complex"
Type Complex {

    -- raw attributes
    re :: double
    im :: double

    -- derived attributes: can refer to the raw attributes
    -- the angle with the real axis
    arg :: double -- might be able to omit this line if we have type inference
    arg = atan2(re, im)

    norm :: double
    norm = sqrt(re * re + im * im)  # the length

    isReal, isImag :: Bool
    isReal = im == 0
    isImag = re == 0
}
