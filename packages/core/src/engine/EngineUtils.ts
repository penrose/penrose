// Utils that are unrelated to the engine, but autodiff/opt/etc only

import { Gradient } from "@penrose/optimizer";
import { Circle } from "../shapes/Circle";
import { Ellipse } from "../shapes/Ellipse";
import { Equation } from "../shapes/Equation";
import { Group } from "../shapes/Group";
import { Image } from "../shapes/Image";
import { Line } from "../shapes/Line";
import { Path as PathShape } from "../shapes/Path";
import { Polygon } from "../shapes/Polygon";
import { Polyline } from "../shapes/Polyline";
import { Rectangle } from "../shapes/Rectangle";
import { InputMeta } from "../shapes/Samplers";
import { Shape } from "../shapes/Shapes";
import { Text } from "../shapes/Text";
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
import {
  Arrow,
  CanPassthrough,
  Center,
  Corner,
  Fill,
  Named,
  Poly,
  Rect,
  Rotate,
  Scale,
  String as StringProps,
  Stroke,
} from "../types/shapes";
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
  PtListV,
  ShapeListV,
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

function mapShape<T, S>(f: (arg: T) => S, v: Shape<T>): Shape<S> {
  switch (v.shapeType) {
    case "Circle":
      return mapCircle(f, v);
    case "Ellipse":
      return mapEllipse(f, v);
    case "Equation":
      return mapEquation(f, v);
    case "Image":
      return mapImage(f, v);
    case "Line":
      return mapLine(f, v);
    case "Path":
      return mapPath(f, v);
    case "Polygon":
      return mapPolygon(f, v);
    case "Polyline":
      return mapPolyline(f, v);
    case "Rectangle":
      return mapRectangle(f, v);
    case "Text":
      return mapText(f, v);
    case "Group":
      return mapGroup(f, v);
  }
}

function mapShapeList<T, S>(f: (arg: T) => S, v: ShapeListV<T>): ShapeListV<S> {
  return {
    tag: "ShapeListV",
    contents: v.contents.map((shape) => {
      return mapShape(f, shape);
    }),
  };
}

const mapCircle = <T, S>(f: (arg: T) => S, v: Circle<T>): Circle<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapFill(f, v),
    ...mapCenter(f, v),
    r: mapFloat(f, v.r),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapEllipse = <T, S>(f: (arg: T) => S, v: Ellipse<T>): Ellipse<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapFill(f, v),
    ...mapCenter(f, v),
    rx: mapFloat(f, v.rx),
    ry: mapFloat(f, v.ry),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapEquation = <T, S>(f: (arg: T) => S, v: Equation<T>): Equation<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapFill(f, v),
    ...mapCenter(f, v),
    ...mapRect(f, v),
    ...mapRotate(f, v),
    ...mapString(f, v),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapGroup = <T, S>(f: (arg: T) => S, v: Group<T>): Group<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    shapes: mapShapeList(f, v.shapes),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapImage = <T, S>(f: (arg: T) => S, v: Image<T>): Image<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapCenter(f, v),
    ...mapRect(f, v),
    ...mapRotate(f, v),
    href: v.href,
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapLine = <T, S>(f: (arg: T) => S, v: Line<T>): Line<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapArrow(f, v),
    start: mapVector(f, v.start),
    end: mapVector(f, v.end),
    strokeLinecap: v.strokeLinecap,
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapPath = <T, S>(f: (arg: T) => S, v: PathShape<T>): PathShape<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapFill(f, v),
    ...mapArrow(f, v),
    d: mapPathData(f, v.d),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapPolygon = <T, S>(f: (arg: T) => S, v: Polygon<T>): Polygon<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapFill(f, v),
    ...mapScale(f, v),
    ...mapPoly(f, v),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapPolyline = <T, S>(f: (arg: T) => S, v: Polyline<T>): Polyline<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapFill(f, v),
    ...mapScale(f, v),
    ...mapPoly(f, v),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapRectangle = <T, S>(
  f: (arg: T) => S,
  v: Rectangle<T>
): Rectangle<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapFill(f, v),
    ...mapCenter(f, v),
    ...mapRotate(f, v),
    ...mapRect(f, v),
    ...mapCorner(f, v),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapText = <T, S>(f: (arg: T) => S, v: Text<T>): Text<S> => {
  return {
    ...v,
    ...mapNamed(f, v),
    ...mapStroke(f, v),
    ...mapFill(f, v),
    ...mapCenter(f, v),
    ...mapRotate(f, v),
    ...mapRect(f, v),
    ...mapString(f, v),
    ascent: mapFloat(f, v.ascent),
    descent: mapFloat(f, v.descent),
    passthrough: mapPassthrough(f, v.passthrough),
  };
};

const mapPassthrough = <T, S>(
  f: (arg: T) => S,
  v: Map<string, CanPassthrough<T>>
): Map<string, CanPassthrough<S>> => {
  const vMapped = new Map<string, CanPassthrough<S>>();
  for (const [key, value] of v.entries()) {
    if (value.tag === "StrV") {
      vMapped.set(key, value);
    } else {
      vMapped.set(key, mapFloat(f, value));
    }
  }
  return vMapped;
};

const mapNamed = <T, S>(f: (arg: T) => S, v: Named<T>): Named<S> => {
  // Cannot use "spread" operator since `v` might have more things than just Named.
  return { name: v.name, style: v.style, ensureOnCanvas: v.ensureOnCanvas };
};

const mapStroke = <T, S>(f: (arg: T) => S, v: Stroke<T>): Stroke<S> => {
  return {
    strokeStyle: v.strokeStyle,
    strokeDasharray: v.strokeDasharray,
    strokeWidth: mapFloat(f, v.strokeWidth),
    strokeColor: mapColor(f, v.strokeColor),
  };
};

const mapFill = <T, S>(f: (arg: T) => S, v: Fill<T>): Fill<S> => {
  return { fillColor: mapColor(f, v.fillColor) };
};

const mapCenter = <T, S>(f: (arg: T) => S, v: Center<T>): Center<S> => {
  return { center: mapVector(f, v.center) };
};

const mapRect = <T, S>(f: (arg: T) => S, v: Rect<T>): Rect<S> => {
  return { width: mapFloat(f, v.width), height: mapFloat(f, v.height) };
};

const mapArrow = <T, S>(f: (arg: T) => S, v: Arrow<T>): Arrow<S> => {
  return {
    startArrowhead: v.startArrowhead,
    endArrowhead: v.endArrowhead,
    flipStartArrowhead: v.flipStartArrowhead,
    startArrowheadSize: mapFloat(f, v.startArrowheadSize),
    endArrowheadSize: mapFloat(f, v.endArrowheadSize),
  };
};

const mapCorner = <T, S>(f: (arg: T) => S, v: Corner<T>): Corner<S> => {
  return { cornerRadius: mapFloat(f, v.cornerRadius) };
};

const mapRotate = <T, S>(f: (arg: T) => S, v: Rotate<T>): Rotate<S> => {
  return { rotation: mapFloat(f, v.rotation) };
};

const mapScale = <T, S>(f: (arg: T) => S, v: Scale<T>): Scale<S> => {
  return { scale: mapFloat(f, v.scale) };
};

const mapPoly = <T, S>(f: (arg: T) => S, v: Poly<T>): Poly<S> => {
  return { points: mapPtList(f, v.points) };
};

const mapString = <T, S>(
  f: (arg: T) => S,
  v: StringProps<T>
): StringProps<S> => {
  return { string: v.string, fontSize: v.fontSize };
};

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
    case "ShapeListV":
      return mapShapeList(f, v);
    // non-numeric Value types
    case "BoolV":
    case "StrV":
      return v;
  }
}

export const compileCompGraph = async (
  shapes: Shape<ad.Num>[]
): Promise<ShapeFn> => {
  const vars = [];
  for (const s of shapes) {
    for (const k of Object.keys(s)) {
      // remove shapeType
      if (k === "shapeType") continue;
      // get all values from passthrough
      else if (k === "passthrough") {
        for (const ptVal of s.passthrough.values()) {
          vars.push(...valueADNums(ptVal));
        }
      } else {
        vars.push(...valueADNums(s[k]));
      }
    }
  }
  const compGraph: ad.Graph = secondaryGraph(vars);
  const evalFn = await genCode(compGraph);
  return (xs: number[]): Shape<number>[] => {
    const numbers = evalFn.call(xs).secondary;
    const m = new Map(compGraph.secondary.map((id, i) => [id, numbers[i]]));
    return shapes.map((s: Shape<ad.Num>) =>
      mapShape(
        (x) =>
          safe(
            m.get(safe(compGraph.nodes.get(x), `missing node`)),
            "missing output"
          ),
        s
      )
    );
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
    case "ShapeListV": {
      const vars = [];
      const shapes = v.contents;
      for (const s of shapes) {
        for (const k of Object.keys(s)) {
          // remove shapeType
          if (k === "shapeType") continue;
          // get all values from passthrough
          else if (k === "passthrough") {
            for (const ptVal of s.passthrough.values()) {
              vars.push(...valueADNums(ptVal));
            }
          } else {
            vars.push(...valueADNums(s[k]));
          }
        }
      }
      return vars;
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

//#endregion

export const exprToNumber = (e: Expr<A>): number => {
  if (e.tag === "Fix") {
    return e.contents;
  }
  throw Error("expecting expr to be number");
};
