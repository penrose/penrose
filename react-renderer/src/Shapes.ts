// anchorPointType =
//   ( "AnchorPoint"
//   , M.fromList
//         -- ("x", (FloatT, x_sampler)),
//         -- ("y", (FloatT, y_sampler)),
//       [ ("location", (PtT, pointSampler))
//       , ("name", (StrT, constValue $ StrV "defaultAnchorPoint"))
//       ])

// circType =
//   ( "Circle"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("r", (FloatT, width_sampler))
//       , ("strokeWidth", (FloatT, stroke_sampler))
//       , ("style", (StrT, constValue $ StrV "filled"))
//       , ("strokeStyle", (StrT, constValue $ StrV "solid"))
//       , ("strokeColor", (ColorT, sampleColor))
//       , ("color", (ColorT, sampleColor))
//       , ("name", (StrT, constValue $ StrV "defaultCircle"))
//       ])

// ellipseType =
//   ( "Ellipse"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("rx", (FloatT, width_sampler))
//       , ("ry", (FloatT, height_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("strokeWidth", (FloatT, stroke_sampler))
//       , ("style", (StrT, sampleDiscrete [StrV "filled"]))
//       , ("strokeColor", (ColorT, sampleColor))
//       , ("strokeStyle", (StrT, stroke_style_sampler))
//       , ("color", (ColorT, sampleColor))
//       , ("name", (StrT, constValue $ StrV "defaultEllipse"))
//       ])

// -- When explicitly declared or computed in Style programs, w and h take precedence over fontSize.
// -- Therefore, custom fontSize in Style will only work when w and h are not specified or computed.
// textType =
//   ( "Text"
//   , M.fromList
//       [ ("x", (FloatT, sampleFloatIn (-canvasWidth / 2, canvasWidth / 2)))
//       , ("y", (FloatT, sampleFloatIn (-canvasHeight / 2, canvasHeight / 2)))
//       , ("w", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
//       , ("h", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
//       , ("fontSize", (StrT, constValue $ StrV "12pt"))
//       , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly)) -- Computed
//       , ("string", (StrT, constValue $ StrV "defaultLabelText"))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("style", (StrT, constValue $ StrV "none"))
//       , ("stroke", (StrT, constValue $ StrV "none"))
//       , ("color", (ColorT, constValue $ ColorV black))
//       , ("name", (StrT, constValue $ StrV "defaultCircle"))
//       ])

// arrowType =
//   ( "Arrow"
//   , M.fromList
//       [ ("startX", (FloatT, x_sampler))
//       , ("startY", (FloatT, y_sampler))
//       , ("endX", (FloatT, x_sampler))
//       , ("endY", (FloatT, y_sampler))
//       , ("thickness", (FloatT, sampleFloatIn (5, 15)))
//       , ("style", (StrT, constValue $ StrV "straight"))
//       , ("color", (ColorT, sampleColor))
//       , ("name", (StrT, constValue $ StrV "defaultArrow"))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("arrowheadStyle", (StrT, constValue $ StrV "arrowhead-2"))
//       , ("arrowheadSize", (FloatT, constValue $ FloatV 1.0))
//       ])

// braceType =
//   ( "Brace"
//   , M.fromList
//       [ ("startX", (FloatT, x_sampler))
//       , ("startY", (FloatT, y_sampler))
//       , ("endX", (FloatT, x_sampler))
//       , ("endY", (FloatT, y_sampler))
//       , ("color", (ColorT, sampleColor))
//       , ("thickness", (FloatT, sampleFloatIn (1, 6)))
//       , ("name", (StrT, constValue $ StrV "defaultBrace"))
//       ])

// curveType =
//   ( "Curve"
//   , M.fromList
//         -- These two fields are for storage.
//       [ ("path", (PtListT, constValue $ PtListV [])) -- TODO: sample path
//       , ("polyline", (PtListT, constValue $ PtListV [])) -- TODO: sample path
//       -- Computed
//       , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
//         -- The frontend only uses pathData to draw the curve.
//       , ("pathData", (PathDataT, constValue $ PathDataV [])) -- TODO: sample path
//       , ("strokeWidth", (FloatT, stroke_sampler))
//       , ("style", (StrT, constValue $ StrV "solid"))
//       , ("effect", (StrT, constValue $ StrV "none"))
//       , ("fill", (ColorT, sampleColor)) -- for no fill, set opacity to 0
//       , ("color", (ColorT, sampleColor))
//       , ("leftArrowhead", (BoolT, constValue $ BoolV False))
//       , ("rightArrowhead", (BoolT, constValue $ BoolV False))
//       , ("arrowheadStyle", (StrT, constValue $ StrV "arrowhead-2"))
//       , ("arrowheadSize", (FloatT, constValue $ FloatV 1.0))
//       , ("name", (StrT, constValue $ StrV "defaultCurve"))
//       ])

// lineType =
//   ( "Line"
//   , M.fromList
//       [ ("startX", (FloatT, x_sampler))
//       , ("startY", (FloatT, y_sampler))
//       , ("endX", (FloatT, x_sampler))
//       , ("endY", (FloatT, y_sampler))
//       , ("thickness", (FloatT, sampleFloatIn (5, 15)))
//       , ("leftArrowhead", (BoolT, constValue $ BoolV False))
//       , ("rightArrowhead", (BoolT, constValue $ BoolV False))
//       , ("arrowheadStyle", (BoolT, constValue $ StrV "arrowhead-2"))
//       , ("arrowheadSize", (BoolT, constValue $ FloatV 1.0))
//       , ("color", (ColorT, sampleColor))
//       , ("style", (StrT, constValue $ StrV "solid"))
//       , ("stroke", (StrT, constValue $ StrV "none"))
//       , ("name", (StrT, constValue $ StrV "defaultLine"))
//       ])

// rectType =
//   ( "Rectangle"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("w", (FloatT, width_sampler))
//       , ("h", (FloatT, height_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("color", (ColorT, sampleColor))
//       , ("strokeWidth", (FloatT, stroke_sampler))
//       , ("style", (StrT, constValue $ StrV "filled"))
//       , ("strokeColor", (ColorT, sampleColor))
//       , ("strokeStyle", (StrT, constValue $ StrV "none"))
//       , ("name", (StrT, constValue $ StrV "defaultRect"))
//       ])

// squareType =
//   ( "Square"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("side", (FloatT, width_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//         -- TODO: distinguish between stroke color and fill color everywhere
//       , ("color", (ColorT, sampleColor))
//       , ("style", (StrT, constValue $ StrV "none")) -- TODO: what is this?
//       , ("strokeColor", (ColorT, sampleColor))
//       , ("strokeWidth", (FloatT, constValue $ FloatV 0.0))
//       , ("name", (StrT, constValue $ StrV "defaultSquare"))
//       ])

// parallelogramType =
//   ( "Parallelogram"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler)) -- (x, y) is the bottom-left corner of the parallelogram
//       , ("y", (FloatT, y_sampler))
//       , ("lengthX", (FloatT, width_sampler))
//       , ("lengthY", (FloatT, height_sampler))
//       , ("angle", (FloatT, constValue $ FloatV 0.0))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("color", (ColorT, sampleColor))
//       , ("stroke-style", (StrT, stroke_style_sampler))
//       , ("stroke-color", (ColorT, sampleColor))
//       , ("name", (StrT, constValue $ StrV "defaultParallelogram"))
//       ])

// imageType =
//   ( "Image"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("w", (FloatT, width_sampler))
//       , ("h", (FloatT, height_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("opacity", (FloatT, constValue $ FloatV 1.0))
//       , ("style", (StrT, constValue $ StrV "none"))
//       , ("stroke", (StrT, constValue $ StrV "none"))
//       , ("path", (StrT, constValue $ StrV "missing image path")) -- Absolute path (URL)
//       , ("name", (StrT, constValue $ StrV "defaultImage"))
//       ])
