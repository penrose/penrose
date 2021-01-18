// Utils that are unrelated to the engine, but autodiff/opt/etc only

// TODO: Fix imports
import { varOf, numOf } from "engine/Autodiff";
import * as _ from "lodash";

// TODO: Is there a way to write these mapping/conversion functions with less boilerplate?

// Generic utils for mapping over values

export function mapTup2<T, S>(f: (arg: T) => S, t: [T, T]): [S, S] {
  // TODO: Polygon seems to be getting through with null to the frontend with previously working set examples -- not sure why?
  // TODO: Should do null checks more systematically across the translation
  if (t[0] === null || t[1] === null) {
    return ([varOf(0.0), varOf(0.0)] as unknown) as [S, S];
  }

  return [f(t[0]), f(t[1])];
}

function mapTup3<T, S>(f: (arg: T) => S, t: [T, T, T]): [S, S, S] {
  return [f(t[0]), f(t[1]), f(t[2])];
}

function mapTup4<T, S>(f: (arg: T) => S, t: [T, T, T, T]): [S, S, S, S] {
  return [f(t[0]), f(t[1]), f(t[2]), f(t[3])];
}

function mapTup2LList<T, S>(f: (arg: T) => S, xss: [T, T][][]): [S, S][][] {
  return xss.map((xs) => xs.map((t) => [f(t[0]), f(t[1])]));
}

// Mapping over values

function mapFloat<T, S>(f: (arg: T) => S, v: IFloatV<T>): IFloatV<S> {
  return {
    tag: "FloatV",
    contents: f(v.contents),
  };
}

function mapPt<T, S>(f: (arg: T) => S, v: IPtV<T>): IPtV<S> {
  return {
    tag: "PtV",
    contents: mapTup2(f, v.contents) as [S, S],
  };
}

function mapPtList<T, S>(f: (arg: T) => S, v: IPtListV<T>): IPtListV<S> {
  return {
    tag: "PtListV",
    contents: v.contents.map((t) => mapTup2(f, t)),
  };
}

function mapList<T, S>(f: (arg: T) => S, v: IListV<T>): IListV<S> {
  return {
    tag: "ListV",
    contents: v.contents.map(f),
  };
}

function mapVector<T, S>(f: (arg: T) => S, v: IVectorV<T>): IVectorV<S> {
  return {
    tag: "VectorV",
    contents: v.contents.map(f),
  };
}

function mapTup<T, S>(f: (arg: T) => S, v: ITupV<T>): ITupV<S> {
  return {
    tag: "TupV",
    contents: mapTup2(f, v.contents),
  };
}

function mapLList<T, S>(f: (arg: T) => S, v: ILListV<T>): ILListV<S> {
  return {
    tag: "LListV",
    contents: v.contents.map((e) => e.map(f)),
  };
}

function mapMatrix<T, S>(f: (arg: T) => S, v: IMatrixV<T>): IMatrixV<S> {
  return {
    tag: "MatrixV",
    contents: v.contents.map((e) => e.map(f)),
  };
}

function mapHMatrix<T, S>(f: (arg: T) => S, v: IHMatrixV<T>): IHMatrixV<S> {
  const m = v.contents;
  return {
    tag: "HMatrixV",
    contents: {
      // TODO: This could probably be a generic map over object values
      xScale: f(m.xScale),
      xSkew: f(m.xSkew),
      yScale: f(m.yScale),
      ySkew: f(m.ySkew),
      dx: f(m.dx),
      dy: f(m.dy),
    },
  };
}

function mapPolygon<T, S>(f: (arg: T) => S, v: IPolygonV<T>): IPolygonV<S> {
  const xs0 = mapTup2LList(f, v.contents[0]);
  const xs1 = mapTup2LList(f, v.contents[1]);
  const xs2 = [mapTup2(f, v.contents[2][0]), mapTup2(f, v.contents[2][1])] as [
    [S, S],
    [S, S]
  ];
  const xs3 = v.contents[3].map((e) => mapTup2(f, e));

  return {
    tag: "PolygonV",
    contents: [xs0, xs1, xs2, xs3],
  };
}

function mapElem<T, S>(f: (arg: T) => S, e: Elem<T>): Elem<S> {
  if (e.tag === "Pt" || e.tag === "QuadBezJoin") {
    return {
      tag: e.tag,
      contents: mapTup2(f, e.contents),
    };
  } else if (e.tag === "CubicBezJoin" || e.tag === "QuadBez") {
    return {
      tag: e.tag,
      contents: mapTup2((x) => mapTup2(f, x), e.contents),
    };
  } else if (e.tag === "CubicBez") {
    return {
      tag: e.tag,
      contents: mapTup3((x) => mapTup2(f, x), e.contents),
    };
  } else {
    throw Error("unknown tag in bezier curve type conversion");
  }
}

function mapSubpath<T, S>(f: (arg: T) => S, s: SubPath<T>): SubPath<S> {
  return {
    tag: s.tag,
    contents: s.contents.map((e) => mapElem(f, e)),
  };
}

function mapPathData<T, S>(f: (arg: T) => S, v: IPathDataV<T>): IPathDataV<S> {
  return {
    tag: "PathDataV",
    contents: v.contents.map((e) => mapSubpath(f, e)),
  };
}

function mapColorInner<T, S>(f: (arg: T) => S, v: Color<T>): Color<S> {
  if (v.tag === "RGBA") {
    const rgb = v.contents;
    return {
      tag: "RGBA",
      contents: mapTup4(f, rgb),
    };
  } else if (v.tag === "HSVA") {
    const hsv = v.contents;
    return {
      tag: "HSVA",
      contents: mapTup4(f, hsv),
    };
  } else {
    throw Error("unexpected color tag");
  }
}

function mapColor<T, S>(f: (arg: T) => S, v: IColorV<T>): IColorV<S> {
  return {
    tag: "ColorV",
    contents: mapColorInner(f, v.contents),
  };
}

function mapPalette<T, S>(f: (arg: T) => S, v: IPaletteV<T>): IPaletteV<S> {
  return {
    tag: "PaletteV",
    contents: v.contents.map((e) => mapColorInner(f, e)),
  };
}

// Utils for converting types of values

// Expects `f` to be a function between numeric types (e.g. number -> VarAD, VarAD -> number, AD var -> VarAD ...)
// Coerces any non-numeric types
export function mapValueNumeric<T, S>(f: (arg: T) => S, v: Value<T>): Value<S> {
  const nonnumericValueTypes = [
    "BoolV",
    "StrV",
    "ColorV",
    "PaletteV",
    "FileV",
    "StyleV",
    "IntV",
  ];

  if (v.tag === "FloatV") {
    return mapFloat(f, v);
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
    throw Error(
      `unimplemented conversion from autodiff types for numeric types for value type '${v.tag}'`
    );
  }
}

export const valueAutodiffToNumber = (v: Value<VarAD>): Value<number> =>
  mapValueNumeric(numOf, v);

export const valueNumberToAutodiff = (v: Value<number>): Value<VarAD> =>
  mapValueNumeric(varOf, v);

// Walk translation to convert all TagExprs (tagged Done or Pending) in the state to VarADs
// (This is because, when decoded from backend, it's not yet in VarAD form -- although this code could be phased out if the translation becomes completely generated in the frontend)

export function mapTagExpr<T, S>(f: (arg: T) => S, e: TagExpr<T>): TagExpr<S> {
  if (e.tag === "Done") {
    return {
      tag: "Done",
      contents: mapValueNumeric(f, e.contents),
    };
  } else if (e.tag === "Pending") {
    return {
      tag: "Pending",
      contents: mapValueNumeric(f, e.contents),
    };
  } else if (e.tag === "OptEval") {
    // We don't convert expressions because any numbers encountered in them will be converted by the evaluator (to VarAD) as needed
    // TODO: Need to convert expressions to numbers, or back to varying? I guess `varyingPaths` is the source of truth
    return e;
  } else {
    throw Error("unrecognized tag");
  }
}

export function mapGPIExpr<T, S>(f: (arg: T) => S, e: GPIExpr<T>): GPIExpr<S> {
  const propDict = Object.entries(e[1]).map(([prop, val]) => [
    prop,
    mapTagExpr(f, val),
  ]);

  return [e[0], Object.fromEntries(propDict)];
}

export function mapTranslation<T, S>(
  f: (arg: T) => S,
  trans: ITrans<T>
): ITrans<S> {
  const newTrMap = Object.entries(trans.trMap).map(([name, fdict]) => {
    const fdict2 = Object.entries(fdict).map(([prop, val]) => {
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
    trMap: Object.fromEntries(newTrMap),
  };
}

// TODO: Check the input type?
export const makeTranslationDifferentiable = (trans: any): Translation => {
  return mapTranslation(varOf, trans);
};

export const makeTranslationNumeric = (trans: Translation): ITrans<number> => {
  return mapTranslation(numOf, trans);
};


//#region translation operations

const dummySourceLoc = (): SourceLoc => {
  return { line: -1, col: -1 };
};

const floatValToExpr = (e: Value<VarAD>): Expr => {
  if (e.tag !== "FloatV") {
    throw Error("expected to insert vector elem of type float");
  }
  return {
    start: dummySourceLoc(),
    end: dummySourceLoc(),
    tag: "Fix", contents: e.contents.val
    // COMBAK: This apparently held a VarAD before the AFloat grammar change? Is doing ".val" going to break something?
  };
};

const mkPropertyDict = (decls: PropertyDecl[]): { [k: string]: TagExpr<VarAD> } => {
  const gpi = {};

  for (let decl of decls) {
    // TODO (error/warning): Warn if any of these properties are duplicated or do not exist in the shape constructor
    gpi[decl.name.value] = { tag: "OptEval", contents: decl.value };
  }

  return gpi;
};

/**
 * Insert an expression into the translation (mutating it), returning a reference to the mutated translation for convenience
 * @param path path to a field or property
 * @param expr new expression
 * @param initTrans initial translation
 *
 */

// TODO: Test this
export const insertGPI = (path: Path, gpi: IFGPI<VarAD>, trans: Translation): Translation => {
  let name, field;

  switch (path.tag) {
    case "FieldPath": {
      [name, field] = [path.name, path.field];
      // TODO: warning / error here
      trans.trMap[name.contents.value][field.value] = gpi;
      return trans;
    }

    default: {
      throw Error("expected GPI");
    }
  }
};

// This function is a combination of `addField` and `addProperty` from `Style.hs`
export const insertExpr = (path: Path, expr: TagExpr<VarAD>, initTrans: Translation): Translation => {
  const trans = initTrans;
  let name, field, prop;

  // console.log("insertExpr: path, expr", path, expr);
  // COMBAK: Deal with path aliasing for both field and property paths

  switch (path.tag) {
    case "FieldPath": {
      [name, field] = [path.name, path.field];

      // Initialize the field dict if it hasn't been initialized
      if (!trans.trMap[name.contents.value]) {
        trans.trMap[name.contents.value] = {};
      }

      let fexpr: FieldExpr<VarAD> = { tag: "FExpr", contents: expr };

      // If it's a GPI, instantiate it (rule Line-Set-Ctor); otherwise put it in the translation as-is
      if (expr.tag === "OptEval") {
        const gpi: Expr = expr.contents;
        if (gpi.tag === "GPIDecl") {
          const [nm, decls]: [Identifier, PropertyDecl[]] = [gpi.shapeName, gpi.properties];

          fexpr = {
            tag: "FGPI",
            contents: [nm.value, mkPropertyDict(decls)]
          };
        }
      }

      // For any non-GPI thing, just put it in the translation
      // NOTE: this will overwrite existing expressions
      // TODO: warning / error here
      trans.trMap[name.contents.value][field.value] = fexpr;
      return trans;
    }

    case "PropertyPath": {
      [name, field, prop] = [path.name, path.field, path.property];

      if (!trans.trMap[name.contents.value]) {
        trans.trMap[name.contents.value] = {};
      }

      const gpi: FieldExpr<IVarAD> = trans.trMap[name.contents.value][field.value];

      if (gpi.tag === "FExpr") {
        // TODO (error)
        throw Error("expected GPI");
      } else if (gpi.tag === "FGPI") {
        const [, properties] = gpi.contents;
        properties[prop.value] = expr;
        return trans;
      } else { throw Error("unexpected tag"); }
    }

    case "AccessPath": {
      const [innerPath, indices] = [path.path, path.indices];

      switch (innerPath.tag) {
        case "FieldPath": {
          // a.x[0] = e
          [name, field] = [innerPath.name, innerPath.field];
          const res = trans.trMap[name.contents.value][field.value];
          if (res.tag !== "FExpr") {
            throw Error("did not expect GPI in vector access");
          }
          const res2: TagExpr<IVarAD> = res.contents;
          // Deal with vector expressions
          if (res2.tag === "OptEval") {
            const res3: Expr = res2.contents;
            if (res3.tag !== "Vector") {
              throw Error("expected Vector");
            }
            const res4: Expr[] = res3.contents;

            if (expr.tag === "OptEval") {
              res4[indices[0]] = expr.contents;
            } else if (expr.tag === "Done") {
              res4[indices[0]] = floatValToExpr(expr.contents);
            } else {
              throw Error("unexpected pending val");
            }

            return trans;
          } else if (res2.tag === "Done") {
            // Deal with vector values
            const res3: Value<IVarAD> = res2.contents;
            if (res3.tag !== "VectorV") {
              throw Error("expected Vector");
            }
            const res4: IVarAD[] = res3.contents;

            if (expr.tag === "Done" && expr.contents.tag === "FloatV") {
              res4[indices[0]] = expr.contents.contents;
            } else {
              throw Error("unexpected val");
            }

            return trans;
          } else {
            throw Error("unexpected tag");
          }
        }

        case "PropertyPath": {
          const ip = innerPath as IPropertyPath;
          // a.x.y[0] = e
          [name, field, prop] = [ip.name, ip.field, ip.property];
          const gpi = trans.trMap[name.contents.value][field.value] as IFGPI<VarAD>;
          const [, properties] = gpi.contents;
          const res = properties[prop.value];

          if (res.tag === "OptEval") {
            // Deal with vector expresions
            const res2 = res.contents;
            if (res2.tag !== "Vector") {
              throw Error("expected Vector");
            }
            const res3 = res2.contents;

            if (expr.tag === "OptEval") {
              res3[indices[0]] = expr.contents;
            } else if (expr.tag === "Done") {
              res3[indices[0]] = floatValToExpr(expr.contents);
            } else {
              throw Error("unexpected pending val");
            }

            return trans;
          } else if (res.tag === "Done") {
            // Deal with vector values
            const res2 = res.contents;
            if (res2.tag !== "VectorV") {
              throw Error("expected Vector");
            }
            const res3 = res2.contents;

            if (expr.tag === "Done" && expr.contents.tag === "FloatV") {
              res3[indices[0]] = expr.contents.contents;
            } else {
              throw Error("unexpected val");
            }

            return trans;
          } else {
            throw Error("unexpected tag");
          }
        }

        default:
          throw Error("should not have nested AccessPath in AccessPath");
      }
    }
  }

  throw Error("shouldn't be reached");
};

// Mutates translation
export const insertExprs = (ps: Path[], es: TagExpr<VarAD>[], tr: Translation): Translation => {
  if (ps.length !== es.length) { throw Error("length should be the same"); }

  let tr2 = tr;
  for (let i = 0; i < ps.length; i++) {
    // Tr gets mutated
    tr2 = insertExpr(ps[i], es[i], tr);
  }

  return tr2;
};

/**
 * Finds an expression in a translation given a field or property path.
 * @param trans - a translation from `State`
 * @param path - a path to an expression
 * @returns an expression
 *
 * TODO: the optional type here exist because GPI is not an expression in Style yet. It's not the most sustainable pattern w.r.t to our current way to typecasting the result using `as`.
 */
export const findExpr = (
  trans: Translation,
  path: Path
): TagExpr<VarAD> | IFGPI<VarAD> => {
  let name, field, prop;

  switch (path.tag) {
    case "FieldPath":
      [name, field] = [path.name.contents.value, path.field.value];
      // Type cast to field expression
      const fieldExpr = trans.trMap[name][field];

      if (!fieldExpr) {
        throw Error(
          `Could not find field '${JSON.stringify(path)}' in translation`
        );
      }

      switch (fieldExpr.tag) {
        case "FGPI":
          return fieldExpr;
        case "FExpr":
          return fieldExpr.contents;
      }

    case "PropertyPath":
      [name, field, prop] = [path.name.contents.value, path.field.value, path.property.value];
      // Type cast to FGPI and get the properties
      const gpi = trans.trMap[name][field];

      if (!gpi) {
        throw Error(
          `Could not find GPI '${JSON.stringify(path)}' in translation`
        );
      }

      switch (gpi.tag) {
        case "FExpr":
          throw new Error("field path leads to an expression, not a GPI");
        case "FGPI":
          const [, propDict] = gpi.contents;
          return propDict[prop];
      }

    case "AccessPath": {
      // Have to look up AccessPaths first, since they make a recursive call, and are not invalid paths themselves 
      const res = findExpr(trans, path.path);
      const i = path.indices[0]; // COMBAK VECTORS: Currently only supports 1D vectors

      if (res.tag === "OptEval") {
        if (res.contents.tag === "Vector") {
          return res.contents[i];

        } else throw Error("access path lookup is invalid");
      } else if (res.tag === "Done") {
        if (res.contents.tag === "VectorV") {
          return res.contents[i];

        } else throw Error("access path lookup is invalid");
      } else throw Error("access path lookup is invalid");
      // COMBAK ERROR (see `lookupPath` in `GenOptProblem.hs`)
    }
  }

  throw Error("shouldn't be reached");
};

//#endregion
