// Utils that are unrelated to the engine, but autodiff/opt/etc only

import { Gradient } from "@penrose/optimizer";
import _ from "lodash";
import { InputMeta } from "../shapes/Samplers";
import { ShapeDef, shapedefs } from "../shapes/Shapes";
import * as ad from "../types/ad";
import {
  A,
  ASTNode,
  ConcreteNode,
  Identifier,
  NodeType,
  SourceLoc,
} from "../types/ast";
import { StyleError } from "../types/errors";
import { Shape, ShapeAD } from "../types/shape";
import { ShapeFn } from "../types/state";
import { Expr, Path } from "../types/style";
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
} from "../types/value";
import { safe } from "../utils/Util";
import { fns, genCode, input, makeGraph, secondaryGraph } from "./Autodiff";
import { mul } from "./AutodiffFunctions";

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

export const compileCompGraph = async (shapes: ShapeAD[]): Promise<ShapeFn> => {
  const vars = [];
  for (const s of shapes) {
    for (const v of Object.values(s.properties)) {
      vars.push(...valueADNums(v));
    }
  }
  const compGraph: ad.Graph = secondaryGraph(vars);
  const evalFn = await genCode(compGraph);
  return (xs: number[]): Shape[] => {
    const numbers = evalFn.call(xs).secondary;
    const m = new Map(compGraph.secondary.map((id, i) => [id, numbers[i]]));
    return shapes.map((s: ShapeAD) => ({
      ...s,
      properties: _.mapValues(s.properties, (p: Value<ad.Num>) =>
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

// weight for constraints
const constraintWeight = 10e4; // HACK: constant constraint weight
// const constraintWeight = 1; // TODO: If you want to minimally satisfify the constraint. Figure out which one works better wrt `initConstraintWeight`, as the constraint weight is increased by the growth factor anyway

/**
 * Generate an energy function from the current state (using `ad.Num`s only)
 */
export const genGradient = async (
  inputs: InputMeta[],
  objEngs: ad.Num[],
  constrEngs: ad.Num[]
): Promise<Gradient> => {
  // TODO: Doesn't reuse compiled function for now (since caching function in App currently does not work)
  // Compile objective and gradient

  // This changes with the EP round, gets bigger to weight the constraints
  // Therefore it's marked as an input to the generated objective function, which can be partially applied with the ep weight
  // But its initial `val` gets compiled away, so we just set it to zero here
  const epWeightNode = input({ val: 0, key: inputs.length });

  // Note there are two energies, each of which does NOT know about its children, but the root nodes should now have parents up to the objfn energies. The computational graph can be seen in inspecting varyingValuesTF's parents
  // The energies are in the val field of the results (w/o grads)

  // This is fixed during the whole optimization
  const constrWeightNode: ad.Num = constraintWeight;

  // F(x) = o(x) + c0 * penalty * c(x)
  const objs = objEngs.map((x, i) => {
    const secondary = [];
    secondary[i] = x;
    return makeGraph({ primary: x, secondary });
  });
  const constrs = constrEngs.map((x, i) => {
    const secondary = [];
    secondary[objEngs.length + i] = x;
    return makeGraph({
      primary: mul(fns.toPenalty(x), mul(constrWeightNode, epWeightNode)),
      secondary,
    });
  });

  return await genCode(...objs, ...constrs);
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
