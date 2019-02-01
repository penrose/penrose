let p = [Pt (0.0, 0.0), CubicBez ((10.0, 10.0), (20.0, 10.0), (30.0, 0.0))]
let c = [Open p]

let p' = head $ expandCurves p
let x  = polyCubicBez 0 1 p'
