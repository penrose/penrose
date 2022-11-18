// Utils that are unrelated to the engine, but autodiff/opt/etc only

import { LbfgsParams } from "@penrose/optimizer";
import { mapValues } from "lodash";
import { ShapeDef, shapedefs } from "shapes/Shapes";
import * as ad from "types/ad";
import {
  A,
  ASTNode,
  ConcreteNode,
  Identifier,
  NodeType,
  SourceLoc,
} from "types/ast";
import { StyleError } from "types/errors";
import { Shape, ShapeAD } from "types/shape";
import { ShapeFn } from "types/state";
import { Expr, Path } from "types/style";
import {
  Color,
  ColorV,
  FloatV,
  ListV,
  LListV,
  MatrixV,
  PathCmd,
  PathDataV,
  PropID,
  PtListV,
  ShapeTypeStr,
  SubPath,
  TupV,
  Value,
  VectorV,
} from "types/value";
import { safe } from "utils/Util";
import { genCode, secondaryGraph } from "./Autodiff";

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

function mapFloat<T, S>(f: (arg: T) => S, v: FloatV<T>): FloatV<S> {
  return {
    tag: "FloatV",
    contents: f(v.contents),
  };
}

function mapPtList<T, S>(f: (arg: T) => S, v: PtListV<T>): PtListV<S> {
  return {
    tag: "PtListV",
    contents: mapTupNested(f, v.contents),
  };
}

function mapList<T, S>(f: (arg: T) => S, v: ListV<T>): ListV<S> {
  return {
    tag: "ListV",
    contents: v.contents.map(f),
  };
}

function mapVector<T, S>(f: (arg: T) => S, v: VectorV<T>): VectorV<S> {
  return {
    tag: "VectorV",
    contents: v.contents.map(f),
  };
}

function mapTup<T, S>(f: (arg: T) => S, v: TupV<T>): TupV<S> {
  return {
    tag: "TupV",
    contents: mapTuple(f, v.contents),
  };
}

function mapLList<T, S>(f: (arg: T) => S, v: LListV<T>): LListV<S> {
  return {
    tag: "LListV",
    contents: v.contents.map((e) => e.map(f)),
  };
}

function mapMatrix<T, S>(f: (arg: T) => S, v: MatrixV<T>): MatrixV<S> {
  return {
    tag: "MatrixV",
    contents: v.contents.map((e) => e.map(f)),
  };
}

// convert all `ad.Num`s to numbers for use in Shape def
function mapPathData<T, S>(f: (arg: T) => S, v: PathDataV<T>): PathDataV<S> {
  return {
    tag: "PathDataV",
    contents: v.contents.map((pathCmd: PathCmd<T>) => {
      return {
        cmd: pathCmd.cmd,
        contents: pathCmd.contents.map(
          (subCmd: SubPath<T>): SubPath<S> => {
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
      return { tag: v.tag, contents: mapTuple(f, v.contents) };
    case "HSVA":
      return { tag: v.tag, contents: mapTuple(f, v.contents) };
    case "NONE":
      return { tag: v.tag };
  }
}

function mapColor<T, S>(f: (arg: T) => S, v: ColorV<T>): ColorV<S> {
  return {
    tag: "ColorV",
    contents: mapColorInner(f, v.contents),
  };
}

// Utils for converting types of values

// Expects `f` to be a function between numeric types (e.g. number -> ad.Num, ad.Num -> number, AD var -> ad.Num ...)
// Coerces any non-numeric types
export function mapValueNumeric<T, S>(f: (arg: T) => S, v: Value<T>): Value<S> {
  switch (v.tag) {
    case "FloatV":
      return mapFloat(f, v);
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
    case "ColorV":
      return mapColor(f, v);
    case "PathDataV":
      return mapPathData(f, v);
    // non-numeric Value types
    case "BoolV":
    case "StrV":
      return v;
  }
}

export const compileCompGraph = (shapes: ShapeAD[]): ShapeFn => {
  const vars = [];
  for (const s of shapes) {
    for (const v of Object.values(s.properties)) {
      vars.push(...valueADNums(v));
    }
  }
  const compGraph: ad.Graph = secondaryGraph(vars);
  const evalFn = genCode(compGraph);
  return (xs: number[]): Shape[] => {
    const numbers = evalFn.call(xs).secondary;
    const m = new Map(compGraph.secondary.map((id, i) => [id, numbers[i]]));
    return shapes.map((s: ShapeAD) => ({
      ...s,
      properties: mapValues(s.properties, (p: Value<ad.Num>) =>
        mapValueNumeric(
          (x) =>
            safe(
              m.get(
                safe(compGraph.nodes.get(x), `missing node for value ${p.tag}`)
              ),
              "missing output"
            ),
          p
        )
      ),
    }));
  };
};

const valueADNums = (v: Value<ad.Num>): ad.Num[] => {
  switch (v.tag) {
    case "FloatV": {
      return [v.contents];
    }
    case "BoolV":
    case "StrV": {
      return [];
    }
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
      return colorADNums(v.contents);
    }
  }
};

const colorADNums = (c: Color<ad.Num>): ad.Num[] => {
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

//#region translation operations

export const dummySourceLoc = (): SourceLoc => {
  return { line: -1, col: -1 };
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

// Given 'propType' and 'shapeType', return all props of that ValueType
// COMBAK: Model "FloatT", "FloatV", etc as types for ValueType
export const propertiesOf = (
  propType: string,
  shapeType: ShapeTypeStr
): PropID[] => {
  const shapedef: ShapeDef = shapedefs[shapeType];
  const shapeInfo: [string, Value<ad.Num>["tag"]][] = Object.entries(
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
  const shapeInfo: [string, Value<ad.Num>["tag"]][] = Object.entries(
    shapedef.propTags
  );
  return shapeInfo
    .filter(([, tag]) => tag !== propType)
    .map(([pName]) => pName);
};

//#endregion

export const exprToNumber = (e: Expr<A>): number => {
  if (e.tag === "Fix") {
    return e.contents;
  }
  throw Error("expecting expr to be number");
};

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
