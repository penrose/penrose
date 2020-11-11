// Utils that are unrelated to the engine, but autodiff/opt/etc only

// TODO: Fix imports
import { varOf, numOf } from "engine/Autodiff";

// TODO: Is there a way to write these mapping/conversion functions with less boilerplate?

// Generic utils for mapping over values

export function mapTup2<T, S>(
  f: (arg: T) => S,
  t: [T, T]
): [S, S] {
  // TODO: Polygon seems to be getting through with null to the frontend with previously working set examples -- not sure why?
  // TODO: Should do null checks more systematically across the translation
  if (t[0] === null || t[1] === null) {
    console.error("null elements in polygon");
    return [varOf(0.0), varOf(0.0)] as unknown as [S, S];
  }

  return [f(t[0]), f(t[1])];
};

function mapTup3<T, S>(
  f: (arg: T) => S,
  t: [T, T, T]
): [S, S, S] {
  return [f(t[0]), f(t[1]), f(t[2])];
};

function mapTup4<T, S>(
  f: (arg: T) => S,
  t: [T, T, T, T]
): [S, S, S, S] {
  return [f(t[0]), f(t[1]), f(t[2]), f(t[3])];
};

function mapTup2LList<T, S>(
  f: (arg: T) => S,
  xss: [T, T][][]
): [S, S][][] {
  return xss.map(xs => xs.map(t => [f(t[0]), f(t[1])]));
};

// Mapping over values

function mapFloat<T, S>(
  f: (arg: T) => S,
  v: IFloatV<T>
): IFloatV<S> {
  return {
    tag: "FloatV",
    contents: f(v.contents)
  };
};

function mapInt<T, S>(
  f: (arg: T) => S,
  v: IIntV<T>
): IFloatV<S> {
  return {
    tag: "FloatV",
    contents: f(v.contents as unknown as T) as S // Hack because we know that Ints only contain `number`s
  };
};

function mapPt<T, S>(
  f: (arg: T) => S,
  v: IPtV<T>
): IPtV<S> {
  return {
    tag: "PtV",
    contents: mapTup2(f, v.contents) as [S, S]
  };
};

function mapPtList<T, S>(
  f: (arg: T) => S,
  v: IPtListV<T>
): IPtListV<S> {
  return {
    tag: "PtListV",
    contents: v.contents.map(t => mapTup2(f, t))
  };
};

function mapList<T, S>(
  f: (arg: T) => S,
  v: IListV<T>
): IListV<S> {
  return {
    tag: "ListV",
    contents: v.contents.map(f)
  };
};

function mapVector<T, S>(
  f: (arg: T) => S,
  v: IVectorV<T>
): IVectorV<S> {
  return {
    tag: "VectorV",
    contents: v.contents.map(f)
  };
};

function mapTup<T, S>(
  f: (arg: T) => S,
  v: ITupV<T>
): ITupV<S> {
  return {
    tag: "TupV",
    contents: mapTup2(f, v.contents)
  };
};

function mapLList<T, S>(
  f: (arg: T) => S,
  v: ILListV<T>
): ILListV<S> {
  return {
    tag: "LListV",
    contents: v.contents.map(e => e.map(f))
  };
};

function mapMatrix<T, S>(
  f: (arg: T) => S,
  v: IMatrixV<T>
): IMatrixV<S> {
  return {
    tag: "MatrixV",
    contents: v.contents.map(e => e.map(f))
  };
};

function mapHMatrix<T, S>(
  f: (arg: T) => S,
  v: IHMatrixV<T>
): IHMatrixV<S> {
  const m = v.contents;
  return {
    tag: "HMatrixV",
    contents: { // TODO: This could probably be a generic map over object values
      xScale: f(m.xScale),
      xSkew: f(m.xSkew),
      yScale: f(m.yScale),
      ySkew: f(m.ySkew),
      dx: f(m.dx),
      dy: f(m.dy),
    }
  };
};

function mapPolygon<T, S>(
  f: (arg: T) => S,
  v: IPolygonV<T>
): IPolygonV<S> {
  const xs0 = mapTup2LList(f, v.contents[0]);
  const xs1 = mapTup2LList(f, v.contents[1]);
  const xs2 = [mapTup2(f, v.contents[2][0]), mapTup2(f, v.contents[2][1])] as [[S, S], [S, S]];
  const xs3 = v.contents[3].map(e => mapTup2(f, e));

  return {
    tag: "PolygonV",
    contents: [xs0, xs1, xs2, xs3]
  };
};

function mapElem<T, S>(
  f: (arg: T) => S,
  e: Elem<T>
): Elem<S> {
  if (e.tag === "Pt" || e.tag === "QuadBezJoin") {
    return {
      tag: e.tag,
      contents: mapTup2(f, e.contents)
    };
  } else if (e.tag === "CubicBezJoin" || e.tag === "QuadBez") {
    return {
      tag: e.tag,
      contents: mapTup2(x => mapTup2(f, x), e.contents)
    };
  } else if (e.tag === "CubicBez") {
    return {
      tag: e.tag,
      contents: mapTup3(x => mapTup2(f, x), e.contents)
    };
  } else {
    throw Error("unknown tag in bezier curve type conversion");
  }
};

function mapSubpath<T, S>(
  f: (arg: T) => S,
  s: SubPath<T>
): SubPath<S> {
  return {
    tag: s.tag,
    contents: s.contents.map(e => mapElem(f, e))
  };
};

function mapPathData<T, S>(
  f: (arg: T) => S,
  v: IPathDataV<T>
): IPathDataV<S> {
  return {
    tag: "PathDataV",
    contents: v.contents.map(e => mapSubpath(f, e))
  };
};

function mapColorInner<T, S>(
  f: (arg: T) => S,
  v: Color<T>
): Color<S> {
  if (v.tag === "RGBA") {
    const rgb = v.contents;
    return {
      tag: "RGBA",
      contents: mapTup4(f, rgb)
    };
  } else if (v.tag === "HSVA") {
    const hsv = v.contents;
    return {
      tag: "HSVA",
      contents: mapTup4(f, hsv)
    };
  } else {
    throw Error("unexpected color tag");
  }
};

function mapColor<T, S>(
  f: (arg: T) => S,
  v: IColorV<T>
): IColorV<S> {
  return {
    tag: "ColorV",
    contents: mapColorInner(f, v.contents)
  };
};

function mapPalette<T, S>(
  f: (arg: T) => S,
  v: IPaletteV<T>
): IPaletteV<S> {
  return {
    tag: "PaletteV",
    contents: v.contents.map(e => mapColorInner(f, e))
  };
};

// Utils for converting types of values

// Expects `f` to be a function between numeric types (e.g. number -> VarAD, VarAD -> number, AD var -> VarAD ...)
// Coerces any non-numeric types
export function mapValueNumeric<T, S>(
  f: (arg: T) => S,
  v: Value<T>
): Value<S> {
  const nonnumericValueTypes = ["BoolV", "StrV", "ColorV", "PaletteV", "FileV", "StyleV"];

  if (v.tag === "FloatV") {
    return mapFloat(f, v);
  } else if (v.tag === "IntV") {
    // TODO: only convert ints if they are used in place of floats (e.g. shape properties that have float type)
    // This converts ints to floats, losing the int information (e.g. on roundtrip)
    return mapInt(f, v);
  } else if (v.tag === "PtV") {
    return mapPt(f, v);
  } else if (v.tag === "PtListV") {
    return mapPtList(f, v);
  } else if (v.tag === "ListV") {
    return mapList(f, v);
  } else if (v.tag === "VectorV") {
    return mapVector(f, v);
  } else if (v.tag === "MatrixV") {
    return mapMatrix(f, v);
  } else if (v.tag === "TupV") {
    return mapTup(f, v);
  } else if (v.tag === "LListV") {
    return mapLList(f, v);
  } else if (v.tag === "HMatrixV") {
    return mapHMatrix(f, v);
  } else if (v.tag === "PolygonV") {
    return mapPolygon(f, v);
  } else if (v.tag === "ColorV") {
    return mapColor(f, v);
  } else if (v.tag === "PaletteV") {
    return mapPalette(f, v);
  } else if (v.tag === "PathDataV") {
    return mapPathData(f, v);
  } else if (nonnumericValueTypes.includes(v.tag)) {
    return v as Value<S>;
  } else {
    throw Error(`unimplemented conversion from autodiff types for numeric types for value type '${v.tag}'`);
  }
};

export const valueAutodiffToNumber = (v: Value<VarAD>): Value<number> => mapValueNumeric(numOf, v);

export const valueNumberToAutodiff = (v: Value<number>): Value<VarAD> => mapValueNumeric(varOf, v);

// Walk translation to convert all TagExprs (tagged Done or Pending) in the state to VarADs
// (This is because, when decoded from backend, it's not yet in VarAD form -- although this code could be phased out if the translation becomes completely generated in the frontend)

export function mapTagExpr<T, S>(
  f: (arg: T) => S,
  e: TagExpr<T>
): TagExpr<S> {
  if (e.tag === "Done") {
    return {
      tag: "Done",
      contents: mapValueNumeric(f, e.contents)
    };
  } else if (e.tag === "Pending") {
    return {
      tag: "Pending",
      contents: mapValueNumeric(f, e.contents)
    };
  } else if (e.tag === "OptEval") {
    // We don't convert expressions because any numbers encountered in them will be converted by the evaluator (to VarAD) as needed
    // TODO: Need to convert expressions to numbers, or back to varying? I guess `varyingPaths` is the source of truth
    return e;
  } else {
    throw Error("unrecognized tag");
  }
};

export function mapGPIExpr<T, S>(
  f: (arg: T) => S,
  e: GPIExpr<T>
): GPIExpr<S> {
  const propDict = Object.entries(e[1])
    .map(([prop, val]) =>
      [prop, mapTagExpr(f, val)]
    );

  return [e[0], Object.fromEntries(propDict)];
};

export function mapTranslation<T, S>(
  f: (arg: T) => S,
  trans: ITrans<T>
): ITrans<S> {
  const newTrMap = Object.entries(trans.trMap)
    .map(([name, fdict]) => {

      const fdict2 = Object.entries(fdict)
        .map(([prop, val]) => {
          if (val.tag === "FExpr") {
            return [prop, { tag: "FExpr", contents: mapTagExpr(f, val.contents) }];
          } else if (val.tag === "FGPI") {
            return [prop, { tag: "FGPI", contents: mapGPIExpr(f, val.contents) }];
          } else {
            console.log(prop, val);
            throw Error("unknown tag on field expr");
          }
        });

      return [name, Object.fromEntries(fdict2)];
    });

  return {
    ...trans,
    trMap: Object.fromEntries(newTrMap)
  };
};

// TODO: Check the input type?
export const makeTranslationDifferentiable = (trans: any): Translation => {
  return mapTranslation(varOf, trans);
};

export const makeTranslationNumeric = (trans: Translation): ITrans<number> => {
  return mapTranslation(numOf, trans);
};
