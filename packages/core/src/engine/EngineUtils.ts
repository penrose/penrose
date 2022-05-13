// Utils that are unrelated to the engine, but autodiff/opt/etc only

import { mapValues } from "lodash";
import rfdc from "rfdc";
import { ShapeDef, shapedefs } from "shapes/Shapes";
import { VarAD } from "types/ad";
import {
  A,
  ASTNode,
  C,
  ConcreteNode,
  Identifier,
  NodeType,
  SourceLoc,
  SyntheticNode,
} from "types/ast";
import { StyleError, Warning } from "types/errors";
import { Shape, ShapeAD } from "types/shape";
import { LbfgsParams } from "types/state";
import {
  AnnoFloat,
  Expr,
  IPropertyPath,
  IVector,
  Path,
  PropertyDecl,
} from "types/style";
import {
  Color,
  FieldExpr,
  GPIExpr,
  IColorV,
  IFGPI,
  IFloatV,
  IHMatrixV,
  IListV,
  ILListV,
  IMatrixV,
  IPaletteV,
  IPathCmd,
  IPathDataV,
  IPtListV,
  IPtV,
  ISubPath,
  ITrans,
  ITupV,
  IVectorV,
  PropID,
  ShapeTypeStr,
  TagExpr,
  Translation,
  Value,
} from "types/value";
import { showError } from "utils/Error";
import { safe } from "utils/Util";
import { genCode, secondaryGraph } from "./Autodiff";
const clone = rfdc({ proto: false, circles: false });

// TODO: Is there a way to write these mapping/conversion functions with less boilerplate?

// For wrapping temp Style errors until figuring out how they should be categorized
export const wrapErr = (s: string): StyleError => {
  return {
    tag: "GenericStyleError",
    messages: [s],
  };
};

// TODO(errors): should these kinds of errors be caught by block statics rather than failing at runtime?
export const runtimeValueTypeError = (
  path: Path<A>,
  expectedType: string,
  actualType: string
): StyleError => {
  return {
    tag: "RuntimeValueTypeError",
    path,
    expectedType,
    actualType,
  };
};
// Generic utils for mapping over values
export function mapTuple<T, S>(f: (arg: T) => S, t: T[]): S[] {
  return t.map((tup) => {
    return f(tup);
  });
}
export function mapTupNested<T, S>(f: (arg: T) => S, t: T[][]): S[][] {
  return t.map((tup) => {
    return mapTuple(f, tup);
  });
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
    contents: mapTuple(f, v.contents),
  };
}

function mapPtList<T, S>(f: (arg: T) => S, v: IPtListV<T>): IPtListV<S> {
  return {
    tag: "PtListV",
    contents: mapTupNested(f, v.contents),
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
    contents: mapTuple(f, v.contents),
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

// convert all IVarADs to numbers for use in Shape def
function mapPathData<T, S>(f: (arg: T) => S, v: IPathDataV<T>): IPathDataV<S> {
  return {
    tag: "PathDataV",
    contents: v.contents.map((pathCmd: IPathCmd<T>) => {
      return {
        cmd: pathCmd.cmd,
        contents: pathCmd.contents.map(
          (subCmd: ISubPath<T>): ISubPath<S> => {
            return {
              tag: subCmd.tag,
              contents: mapTuple(f, subCmd.contents),
            };
          }
        ),
      };
    }),
  };
}

function mapColorInner<T, S>(f: (arg: T) => S, v: Color<T>): Color<S> {
  switch (v.tag) {
    case "RGBA":
      return { tag: v.tag, contents: mapTuple(f, (v as any).contents) };
    case "HSVA":
      return { tag: v.tag, contents: mapTuple(f, (v as any).contents) };
    case "NONE":
      return { tag: v.tag };
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
  switch (v.tag) {
    case "FloatV":
      return mapFloat(f, v);
    case "PtV":
      return mapPt(f, v);
    case "PtListV":
      return mapPtList(f, v);
    case "ListV":
      return mapList(f, v);
    case "VectorV":
      return mapVector(f, v);
    case "MatrixV":
      return mapMatrix(f, v);
    case "TupV":
      return mapTup(f, v);
    case "LListV":
      return mapLList(f, v);
    case "HMatrixV":
      return mapHMatrix(f, v);
    case "ColorV":
      return mapColor(f, v);
    case "PaletteV":
      return mapPalette(f, v);
    case "PathDataV":
      return mapPathData(f, v);
    // non-numeric Value types
    case "BoolV":
    case "StrV":
    case "FileV":
    case "StyleV":
    case "IntV":
      return v as Value<S>;
  }
}

const numOf = (x: VarAD): number => {
  if (typeof x === "number") {
    return x;
  } else {
    throw Error("tried to call numOf on a non-constant VarAD");
  }
};

export const shapeAutodiffToNumber = (shapes: ShapeAD[]): Shape[] => {
  const vars = [];
  for (const s of shapes) {
    for (const v of Object.values(s.properties)) {
      vars.push(...valueVarADs(v));
    }
  }
  const g = secondaryGraph(vars);
  const inputs = [];
  for (const v of g.nodes.keys()) {
    if (typeof v !== "number" && v.tag === "Input") {
      inputs[v.key] = v.val;
    }
  }
  const numbers = genCode(g)(inputs).secondary;
  const m = new Map(g.secondary.map((id, i) => [id, numbers[i]]));
  return shapes.map((s: ShapeAD) => ({
    ...s,
    properties: mapValues(s.properties, (p: Value<VarAD>) =>
      mapValueNumeric(
        (x) =>
          safe(m.get(safe(g.nodes.get(x), "missing node")), "missing output"),
        p
      )
    ),
  }));
};

const valueVarADs = (v: Value<VarAD>): VarAD[] => {
  switch (v.tag) {
    case "FloatV": {
      return [v.contents];
    }
    case "IntV":
    case "BoolV":
    case "StrV":
    case "FileV":
    case "StyleV": {
      return [];
    }
    case "PtV":
    case "ListV":
    case "VectorV":
    case "TupV": {
      return v.contents;
    }
    case "PathDataV": {
      return v.contents.flatMap((pathCmd) =>
        pathCmd.contents.flatMap((subPath) => subPath.contents)
      );
    }
    case "PtListV":
    case "MatrixV":
    case "LListV": {
      return v.contents.flat();
    }
    case "ColorV": {
      return colorVarADs(v.contents);
    }
    case "PaletteV": {
      return v.contents.flatMap(colorVarADs);
    }
    case "HMatrixV": {
      return Object.values(v.contents);
    }
  }
};

const colorVarADs = (c: Color<VarAD>): VarAD[] => {
  switch (c.tag) {
    case "RGBA":
    case "HSVA": {
      return c.contents;
    }
    case "NONE": {
      return [];
    }
  }
};

export const valueAutodiffToNumber = (v: Value<VarAD>): Value<number> =>
  mapValueNumeric(numOf, v);

// Walk translation to convert all TagExprs (tagged Done or Pending) in the state to VarADs
// (This is because, when decoded from backend, it's not yet in VarAD form -- although this code could be phased out if the translation becomes completely generated in the frontend)

export function mapTagExpr<T, S>(f: (arg: T) => S, e: TagExpr<T>): TagExpr<S> {
  switch (e.tag) {
    case "Done":
      return {
        tag: "Done",
        contents: mapValueNumeric(f, e.contents),
      };
    case "Pending":
      return {
        tag: "Pending",
        contents: mapValueNumeric(f, e.contents),
      };
    case "OptEval":
      // We don't convert expressions because any numbers encountered in them will be converted by the evaluator (to VarAD) as needed
      // TODO: Need to convert expressions to numbers, or back to varying? I guess `varyingPaths` is the source of truth
      return e;
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
  const newTrMap: [string, { [k: string]: FieldExpr<S> }][] = Object.entries(
    trans.trMap
  ).map(([name, fdict]) => {
    const fdict2: [string, FieldExpr<S>][] = Object.entries(fdict).map(
      ([prop, val]): [string, FieldExpr<S>] => {
        switch (val.tag) {
          case "FExpr":
            return [
              prop,
              { tag: "FExpr", contents: mapTagExpr(f, val.contents) },
            ];
          case "FGPI":
            return [
              prop,
              { tag: "FGPI", contents: mapGPIExpr(f, val.contents) },
            ];
        }
      }
    );

    return [name, Object.fromEntries(fdict2)];
  });

  return {
    ...trans,
    trMap: Object.fromEntries(newTrMap),
  };
}

export const makeTranslationNumeric = (trans: Translation): ITrans<number> => {
  return mapTranslation(numOf, trans);
};

//#region translation operations

export const dummySourceLoc = (): SourceLoc => {
  return { line: -1, col: -1 };
};

export const dummyASTNode = (o: any, nodeType: NodeType): SyntheticNode => {
  return {
    ...o,
    nodeType,
  };
};

export const isConcrete = (node: ASTNode<A>): node is ConcreteNode =>
  node.nodeType === "Substance" ||
  node.nodeType === "Style" ||
  node.nodeType === "Domain";

// COMBAK: Make fake identifier from string (e.g. if we don't have a source loc, make fake source loc)
export const dummyIdentifier = (
  name: string,
  nodeType: NodeType
): Identifier<A> => {
  return {
    nodeType,
    type: "value",
    value: name,
    tag: "Identifier",
  };
};

const floatValToExpr = (e: Value<VarAD>): Expr<A> => {
  if (e.tag !== "FloatV") {
    throw Error("expected to insert vector elem of type float");
  }

  return {
    nodeType: "SyntheticStyle",
    tag: "VaryAD",
    contents: e.contents,
  };
};

const mkPropertyDict = (
  decls: PropertyDecl<A>[]
): { [k: string]: TagExpr<VarAD> } => {
  const gpi = {};

  for (const decl of decls) {
    // TODO(error/warning): Warn if any of these properties are duplicated or do not exist in the shape constructor
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
export const insertGPI = (
  path: Path<A>,
  gpi: IFGPI<VarAD>,
  trans: Translation
): Translation => {
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

const defaultVec2 = (): Expr<A> => {
  const e1: AnnoFloat<A> = {
    ...dummyASTNode({}, "SyntheticStyle"),
    tag: "Vary",
  };
  const e2: AnnoFloat<A> = {
    ...dummyASTNode({}, "SyntheticStyle"),
    tag: "Vary",
  };
  const v2: IVector<A> = {
    ...dummyASTNode({}, "SyntheticStyle"),
    tag: "Vector",
    contents: [e1, e2],
  };
  return v2;
};

// Given 'propType' and 'shapeType', return all props of that ValueType
// COMBAK: Model "FloatT", "FloatV", etc as types for ValueType
export const propertiesOf = (
  propType: string,
  shapeType: ShapeTypeStr
): PropID[] => {
  const shapedef: ShapeDef = shapedefs[shapeType];
  const shapeInfo: [string, Value<VarAD>["tag"]][] = Object.entries(
    shapedef.propTags
  );
  return shapeInfo
    .filter(([, tag]) => tag === propType)
    .map(([pName]) => pName);
};

// Given 'propType' and 'shapeType', return all props NOT of that ValueType
export const propertiesNotOf = (
  propType: string,
  shapeType: ShapeTypeStr
): PropID[] => {
  const shapedef: ShapeDef = shapedefs[shapeType];
  const shapeInfo: [string, Value<VarAD>["tag"]][] = Object.entries(
    shapedef.propTags
  );
  return shapeInfo
    .filter(([, tag]) => tag !== propType)
    .map(([pName]) => pName);
};

// This function is a combination of `addField` and `addProperty` from `Style.hs`
// `inCompilePhase` = true = put errors and warnings in the translation, otherwise throw them at runtime
// TODO(error/warn): Improve these warning/error messages (especially for overrides) and distinguish the fatal ones
// TODO(error): rewrite to use the same pattern as `findExprSafe`
export const insertExpr = (
  path: Path<A>,
  expr: TagExpr<VarAD>,
  initTrans: Translation,
  compiling = false,
  override = false
): Translation => {
  let trans = initTrans;
  let name, field, prop;

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
        const gpi: Expr<A> = expr.contents;
        if (gpi.tag === "GPIDecl") {
          const [nm, decls]: [Identifier<A>, PropertyDecl<A>[]] = [
            gpi.shapeName,
            gpi.properties,
          ];

          fexpr = {
            tag: "FGPI",
            contents: [nm.value, mkPropertyDict(decls)],
          };
        }
      }

      // Check overrides before initialization
      if (
        compiling &&
        !override &&
        Object.hasOwn(trans.trMap[name.contents.value], field.value)
      ) {
        trans = addWarn(trans, {
          tag: "InsertedPathWithoutOverrideError",
          path,
        }); // TODO(error): Should this be an error?
      }

      // For any non-GPI thing, just put it in the translation
      // NOTE: this will overwrite existing expressions
      trans.trMap[name.contents.value][field.value] = fexpr;
      return trans;
    }

    case "PropertyPath": {
      [name, field, prop] = [path.name, path.field, path.property];

      if (!trans.trMap[name.contents.value]) {
        trans.trMap[name.contents.value] = {};
      }

      const fieldRes: FieldExpr<VarAD> =
        trans.trMap[name.contents.value][field.value];

      // TODO(errors): Catch error if SubObj, etc don't exist -- but should these kinds of errors be caught by block statics rather than failing at runtime?
      if (!fieldRes) {
        // TODO(errors): Catch error if field doesn't exist
        return addWarn(trans, {
          tag: "InsertedPropWithNoFieldError",
          subObj: name,
          field,
          property: prop,
          path,
        });
      }

      if (fieldRes.tag === "FExpr") {
        // Deal with GPI aliasing (i.e. only happens if a GPI is aliased to another, and some operation is performed on the aliased GPI's property, it happens to the original)
        // TODO: Test this
        if (fieldRes.contents.tag === "OptEval") {
          if (fieldRes.contents.contents.tag === "FieldPath") {
            const p = fieldRes.contents.contents;
            if (
              p.name.contents.value === name.contents.value &&
              p.field.value === field.value
            ) {
              if (compiling) {
                return addWarn(trans, { tag: "CircularPathAlias", path });
              }
              // TODO(errors): Use "showErrors" not these ad-hoc errors
              const err = `path was aliased to itself`;
              throw Error(err);
            }
            const newPath = clone(path);
            return insertExpr(
              {
                ...newPath,
                tag: "PropertyPath",
                name: p.name, // Note use of alias
                field: p.field, // Note use of alias
                property: path.property,
              },
              expr,
              trans
            );
          }
        }

        // TODO(error)
        if (compiling) {
          return addWarn(trans, {
            tag: "InsertedPropWithNoGPIError",
            subObj: name,
            field,
            property: prop,
            path,
          });
        }
        const err = `Err: Sub obj '${name.contents.value}' does not have GPI '${field.value}'; cannot add property '${prop.value}'`;
        throw Error(err);
      } else if (fieldRes.tag === "FGPI") {
        const [, properties] = fieldRes.contents;

        // TODO(error): check for field/property overrides of paths that don't already exist
        // TODO(error): if there are multiple matches, override errors behave oddly...
        if (compiling && !override && Object.hasOwn(properties, prop.value)) {
          trans = addWarn(trans, {
            tag: "InsertedPathWithoutOverrideError",
            path,
          });
        }

        properties[prop.value] = expr;
        return trans;
      } else {
        throw Error("unexpected tag");
      }
    }

    // TODO(error): deal with override for accesspaths? I don't know if you can currently write something like `override A.shape.center[0] = 5.` in Style
    case "AccessPath": {
      const [innerPath, indices] = [path.path, path.indices];

      switch (innerPath.tag) {
        case "FieldPath": {
          // a.x[0] = e
          [name, field] = [innerPath.name, innerPath.field];
          const res = trans.trMap[name.contents.value][field.value];
          if (res.tag !== "FExpr") {
            if (compiling) {
              return addWarn(
                trans,
                runtimeValueTypeError(path, "Float", "GPI")
              );
            }
            const err = "did not expect GPI in vector access";
            throw Error(err);
          }
          const res2: TagExpr<VarAD> = res.contents;

          // Deal with vector expressions
          if (res2.tag === "OptEval") {
            const res3: Expr<A> = res2.contents;
            if (res3.tag !== "Vector") {
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Vector", res3.tag)
                );
              }
              const err = "expected Vector";
              throw Error(err);
            }
            const res4: Expr<A>[] = res3.contents;

            if (expr.tag === "OptEval") {
              res4[exprToNumber(indices[0])] = expr.contents;
            } else if (expr.tag === "Done") {
              res4[exprToNumber(indices[0])] = floatValToExpr(expr.contents);
            } else {
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Float", "Pending float")
                );
              }
              const err = "unexpected pending val";
              throw Error(err);
            }

            return trans;
          } else if (res2.tag === "Done") {
            // Deal with vector values
            const res3: Value<VarAD> = res2.contents;
            if (res3.tag !== "VectorV") {
              const err = "expected Vector";
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Vector", "res3.tag")
                );
              }
              throw Error(err);
            }
            const res4: VarAD[] = res3.contents;

            if (expr.tag === "Done" && expr.contents.tag === "FloatV") {
              res4[exprToNumber(indices[0])] = expr.contents.contents;
            } else {
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Float", expr.tag)
                );
              }
              const err = "unexpected val";
              throw Error(err);
            }

            return trans;
          } else {
            if (compiling) {
              // TODO(errors): expected type?
              return addWarn(
                trans,
                runtimeValueTypeError(path, "Float", res2.tag)
              );
            }
            const err = "unexpected tag";
            throw Error(err);
          }
        }

        case "PropertyPath": {
          const ip = innerPath as IPropertyPath<C>;
          // a.x.y[0] = e
          [name, field, prop] = [ip.name, ip.field, ip.property];
          const gpi = trans.trMap[name.contents.value][
            field.value
          ] as IFGPI<VarAD>;
          const [gpiType, properties] = gpi.contents;

          // Right now, a property may not have been initialized (e.g. during the Style interpretation phase, when we are creating a translation).
          // If the expected property is a non-vector type, throw an error, as we can't insert an expression into an uninitialized non-vector (list or other composite type).
          const shapeProps = propertiesOf("VectorV", gpiType);
          if (!shapeProps.includes(prop.value)) {
            return addWarn(trans, {
              tag: "InvalidGPIPropertyError",
              givenProperty: prop,
              expectedProperties: shapeProps,
            });
          }

          // Otherwise, it's a vector type, so if the value hasn't been set, initialize it with a default vector (?, ?)
          // TODO(vec): Note this assumes a 2D vector. Otherwise we wouldn't know the size of the vector to initialize.
          if (!(prop.value in properties)) {
            properties[prop.value] = {
              tag: "OptEval",
              contents: defaultVec2(),
            };
          }

          // Continue as usual with insertion. TODO: Deal with overrides? Not sure how to distinguish a user-set `?` from a default-generated `?`
          const res = properties[prop.value];

          if (res.tag === "OptEval") {
            // Deal with vector expresions
            const res2 = res.contents;
            if (res2.tag !== "Vector") {
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Vector", res2.tag)
                );
              }
              const err = "expected Vector";
              throw Error(err);
            }
            const res3 = res2.contents;

            if (expr.tag === "OptEval") {
              res3[exprToNumber(indices[0])] = expr.contents;
            } else if (expr.tag === "Done") {
              res3[exprToNumber(indices[0])] = floatValToExpr(expr.contents);
            } else {
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Float", expr.tag)
                );
              }
              const err = "unexpected pending val";
              throw Error(err);
            }

            return trans;
          } else if (res.tag === "Done") {
            // Deal with vector values
            const res2 = res.contents;
            if (res2.tag !== "VectorV") {
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Vector", res2.tag)
                );
              }
              const err = "expected Vector";
              throw Error(err);
            }
            const res3 = res2.contents;

            if (expr.tag === "Done" && expr.contents.tag === "FloatV") {
              res3[exprToNumber(indices[0])] = expr.contents.contents;
            } else {
              if (compiling) {
                return addWarn(
                  trans,
                  runtimeValueTypeError(path, "Float", expr.tag)
                );
              }
              const err = "unexpected val";
              throw Error(err);
            }

            return trans;
          } else {
            throw Error("internal error: unexpected tag");
          }
        }

        default:
          throw Error(
            "internal error: should not have nested AccessPath in AccessPath"
          );
      }
    }
  }

  throw Error("internal error: unknown tag");
};

// Mutates translation
export const insertExprs = (
  ps: Path<A>[],
  es: TagExpr<VarAD>[],
  tr: Translation,
  compiling = false,
  override = false
): Translation => {
  if (ps.length !== es.length) {
    throw Error("length should be the same");
  }

  let tr2 = tr;
  for (let i = 0; i < ps.length; i++) {
    // Tr gets mutated
    tr2 = insertExpr(ps[i], es[i], tr, compiling, override);
  }

  return tr2;
};

export const isTagExpr = (e: any): e is TagExpr<VarAD> => {
  return e.tag === "OptEval" || e.tag === "Done" || e.tag === "Pending";
};

// Version of findExpr if you expect to not encounter any errors (e.g., if it's being used after the translation has already been checked)
export const findExprSafe = (
  trans: Translation,
  path: Path<A>
): TagExpr<VarAD> | IFGPI<VarAD> => {
  const res = findExpr(trans, path);
  if (res.tag !== "FGPI" && !isTagExpr(res)) {
    // Is an error
    throw Error(showError(res));
  }

  return res;
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
  path: Path<A>
): TagExpr<VarAD> | IFGPI<VarAD> | StyleError => {
  let name, field, prop;

  switch (path.tag) {
    case "FieldPath": {
      [name, field] = [path.name.contents.value, path.field.value];
      const fields = trans.trMap[name];
      if (!fields) {
        return { tag: "NonexistentNameError", name: path.name.contents, path };
      }

      // Type cast to field expression
      const fieldExpr = fields[field];

      if (!fieldExpr) {
        return { tag: "NonexistentFieldError", field: path.field, path };
      }

      switch (fieldExpr.tag) {
        case "FGPI":
          return fieldExpr;
        case "FExpr":
          return fieldExpr.contents;
      }
      break; // dead code to please ESLint
    }

    case "PropertyPath": {
      [name, field, prop] = [
        path.name.contents.value,
        path.field.value,
        path.property.value,
      ];

      const fieldsP = trans.trMap[name];
      if (!fieldsP) {
        return { tag: "NonexistentNameError", name: path.name.contents, path };
      }

      // Type cast to FGPI and get the properties
      const gpi = fieldsP[field];

      if (!gpi) {
        return { tag: "NonexistentGPIError", gpi: path.field, path };
      }

      switch (gpi.tag) {
        case "FExpr": {
          return { tag: "ExpectedGPIGotFieldError", field: path.field, path };
        }
        case "FGPI": {
          const [, propDict] = gpi.contents;
          const propRes = propDict[prop];
          if (!propRes) {
            return {
              tag: "NonexistentPropertyError",
              property: path.property,
              path,
            };
          }
          return propRes;
        }
      }
      break; // dead code to please ESLint
    }

    case "AccessPath": {
      // Have to look up AccessPaths first, since they make a recursive call, and are not invalid paths themselves
      const res = findExpr(trans, path.path);
      const i = exprToNumber(path.indices[0]); // COMBAK VECTORS: Currently only supports 1D vectors

      if (res.tag === "OptEval") {
        const res2: Expr<A> = res.contents;

        if (res2.tag === "Vector") {
          const inner: Expr<A> = res2.contents[i];
          return { tag: "OptEval", contents: inner };
        } else if (res2.tag === "List") {
          const inner: Expr<A> = res2.contents[i];
          return { tag: "OptEval", contents: inner };
        } else if (res2.tag === "PropertyPath" || res2.tag === "FieldPath") {
          // COMBAK: This deals with accessing elements of path aliases. Maybe there is a nicer way to do it.
          return findExpr(trans, { ...path, path: res2 });
        } else {
          // NOTE: we cannot check because we cannot evaluate right now. Returning the result directly instead
          return res;
        }
      } else if (res.tag === "Done") {
        if (res.contents.tag === "VectorV") {
          const inner: VarAD = res.contents.contents[i];
          return { tag: "Done", contents: { tag: "FloatV", contents: inner } };
        } else {
          return { tag: "InvalidAccessPathError", path };
        }
      } else {
        return { tag: "InvalidAccessPathError", path };
      }
    }
  }

  throw Error("internal error: unknown tag");
};

//#endregion

export const isPath = <T>(expr: Expr<T>): expr is Path<T> => {
  return ["FieldPath", "PropertyPath", "AccessPath", "LocalVar"].includes(
    expr.tag
  );
};

export const exprToNumber = (e: Expr<A>): number => {
  if (e.tag === "Fix") {
    return e.contents;
  }
  throw Error("expecting expr to be number");
};

export const numToExpr = (n: number): Expr<A> => {
  return {
    nodeType: "SyntheticStyle",
    tag: "Fix",
    contents: n,
  };
};

// Add warning to the end of the existing list
export const addWarn = (tr: Translation, warn: Warning): Translation => {
  return {
    ...tr,
    warnings: tr.warnings.concat(warn),
  };
};

// COMBAK: consolidate prettyprinting code

//#region Constants/helpers for the optimization initialization (used by both the compiler and the optimizer)

// Intial weight for constraints
export const initConstraintWeight = 10e-3;

const defaultLbfgsMemSize = 17;

export const defaultLbfgsParams: LbfgsParams = {
  lastState: undefined,
  lastGrad: undefined,
  s_list: [],
  y_list: [],
  numUnconstrSteps: 0,
  memSize: defaultLbfgsMemSize,
};

//#endregion
