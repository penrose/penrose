import { differentiable } from "engine/Autodiff";
import { valueNumberToAutodiff } from "engine/EngineUtils";
import { initConstraintWeight } from "engine/Optimizer";
import { evalShape, evalShapes, insertExpr } from "engine/Evaluator";
import { map, mapKeys, mapValues, zip } from "lodash";
import { canvasSize } from "ui/Canvas";
import { randFloat, randFloats, safe } from "utils/Util";

type ShapeDef = IShapeDef;

// type HasTag<T, N> = T extends { tag: N } ? T : never;

type PropType = Value<number>["tag"];
type PropContents = Value<number>["contents"];
type IPropModel = {
  [k: string]: [PropType, Sampler];
};

interface IShapeDef {
  shapeType: string;
  properties: IPropModel;
}

type ConstSampler = (type: PropType, value: PropContents) => Value<number>;
type Sampler = () => Value<number>;

type Range = [number, number];
const canvasXRange: Range = [-canvasSize[0] / 2, canvasSize[0] / 2];
const canvasYRange: Range = [-canvasSize[1] / 2, canvasSize[1] / 2];

const vectorSampler: Sampler = (): IVectorV<number> => ({
  tag: "VectorV",
  contents: [randFloat(...canvasXRange), randFloat(...canvasYRange)],
});
const widthSampler: Sampler = (): IFloatV<number> => ({
  tag: "FloatV",
  contents: randFloat(3, canvasSize[0] / 6),
});
const heightSampler: Sampler = (): IFloatV<number> => ({
  tag: "FloatV",
  contents: randFloat(3, canvasSize[1] / 6),
});
const strokeSampler: Sampler = (): IFloatV<number> => ({
  tag: "FloatV",
  contents: randFloat(0.5, 3),
});
const colorSampler: Sampler = (): IColorV<number> => {
  const [min, max] = [0.1, 0.9];
  return {
    tag: "ColorV",
    contents: {
      tag: "RGBA",
      contents: [
        randFloat(min, max),
        randFloat(min, max),
        randFloat(min, max),
        0.5,
      ],
    },
  };
};

const constValue: ConstSampler = (tag: PropType, contents: PropContents) =>
  ({
    tag,
    contents,
  } as Value<number>);

export const circleDef: ShapeDef = {
  shapeType: "Circle",
  properties: {
    center: ["VectorV", vectorSampler],
    r: ["FloatV", widthSampler],
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", () => constValue("StrV", "filled")],
    strokeStyle: ["StrV", () => constValue("StrV", "solid")],
    strokeColor: ["ColorV", colorSampler],
    color: ["ColorV", colorSampler],
    name: ["StrV", () => constValue("StrV", "defaultCircle")],
  },
};

export const textDef: ShapeDef = {
  shapeType: "Text",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", () => constValue("FloatV", 0)],
    h: ["FloatV", () => constValue("FloatV", 0)],
    fontSize: ["StrV", () => constValue("StrV", "12pt")],
    rotation: ["FloatV", () => constValue("FloatV", 0)],
    style: ["StrV", () => constValue("StrV", "none")],
    stroke: ["StrV", () => constValue("StrV", "none")],
    color: ["ColorV", () => black],
    name: ["StrV", () => constValue("StrV", "defaultText")],
    string: ["StrV", () => constValue("StrV", "defaultLabelText")],
    // HACK: typechecking is not passing due to Value mismatch. Not sure why
    polygon: ["PolygonV", () => emptyPoly],
  },
};

const black: IColorV<number> = {
  tag: "ColorV",
  contents: {
    tag: "RGBA",
    contents: [0, 0, 0, 1.0],
  },
};

const emptyPoly: IPolygonV<number> = {
  tag: "PolygonV",
  contents: [
    [],
    [],
    [
      [-Infinity, Infinity],
      [-Infinity, Infinity],
    ],
    [],
  ],
};

/**
 * A registry of all types of shape definitions in the Penrose system.
 */
export const shapedefs: ShapeDef[] = [circleDef, textDef];

const findDef = (type: string): ShapeDef => {
  const res = shapedefs.find(({ shapeType }: ShapeDef) => shapeType === type);
  if (res) return res;
  else throw new Error(`${type} is not a valid shape definition.`);
};

//#region shape conversion helpers
const val2float = (val: Value<number>): number => {
  if (val.tag === "FloatV") {
    return val.contents;
  } else {
    throw new Error(`${val} is not a float value.`);
  }
};
const val2Expr = <T>(val: Value<T>): TagExpr<T> => ({
  tag: "Done",
  contents: val,
});
//#endregion

//#region Resampling
// TODO: The current resampling logic is a bit costly. Alternative: map over all varying paths and call sampleField/Property?

/**
 * Resample all shapes properties using their samplers.
 * @param shapes Old shapes
 * @ignore
 */
const sampleShapes = (shapes: Shape[]): Shape[] =>
  shapes.map((shape: Shape) => sampleShape(shape, findDef(shape.shapeType)));

/**
 * Resample all properties of one shape.
 * @param shape existing shape
 * @param shapeDef shape definition
 * @ignore
 */
const sampleShape = (shape: Shape, shapeDef: ShapeDef): Shape => ({
  ...shape,
  properties: mapValues(shape.properties, (_: Value<number>, prop: string) =>
    sampleProperty(prop, shapeDef)
  ),
});

/**
 * Sample a property based on a shape definition.
 * @param property property name
 * @param shapeDef shape definition
 */
const sampleProperty = (
  property: string,
  shapeDef: ShapeDef
): Value<number> => {
  const propModels: IPropModel = shapeDef.properties;
  const sampler = propModels[property];
  if (sampler) return sampler[1]();
  else
    throw new Error(
      `${property} is not a valid property to be sampled for shape ${shapeDef.shapeType}.`
    );
};

/**
 * Sample varying fields, which are assumed to be positional values. They are sampled with the canvas dimensions.
 * @param state State that contains a list of varying paths
 */
const sampleFields = ({ varyingPaths }: State): number[] => {
  const fieldPaths = varyingPaths.filter(
    ({ tag }: Path) => tag === "AccessPath" || tag === "FieldPath"
  );
  return randFloats(fieldPaths.length, canvasXRange);
};

const samplePath = (path: Path, shapes: Shape[]): Value<number> => {
  // HACK: for access and field paths, sample within the canvas width
  if (path.tag === "AccessPath" || path.tag === "FieldPath") {
    return constValue("FloatV", randFloat(...canvasXRange));
  }
  // for property path, use the sampler in shapedef
  else {
    const [{ contents: subName }, field, prop] = path.contents;
    const { shapeType } = safe(
      shapes.find(
        (s: any) => s.properties.name.contents === `${subName}.${field}`
      ),
      `Cannot find shape ${subName}.${field}`
    );
    const shapeDef = findDef(shapeType);
    const sampledProp: Value<number> = sampleProperty(prop, shapeDef);
    return sampledProp;
  }
};

export const resampleOne = (state: State): State => {
  // resample all the uninitialized and varying values
  const { varyingPaths, shapes, uninitializedPaths, params } = state;
  const varyingValues: Value<number>[] = varyingPaths.map((p: Path) =>
    samplePath(p, shapes)
  );
  const uninitValues: Value<VarAD>[] = uninitializedPaths.map((p: Path) =>
    valueNumberToAutodiff(samplePath(p, shapes))
  );

  // update the translation with all uninitialized values (converted to `Done` values)
  const uninitExprs: TagExpr<VarAD>[] = uninitValues.map((v) => val2Expr(v));
  const uninitMap = zip(uninitializedPaths, uninitExprs) as [
    Path,
    TagExpr<number>
  ][];

  const translation: Translation = uninitMap.reduce(
    (tr: Translation, [p, e]: [Path, Expr]) => insertExpr(p, e, tr),
    state.translation
  );

  const sampledState: State = {
    ...state,
    varyingValues: varyingValues.map((v) => val2float(v)),
    translation,
    params: {
      ...params,
      weight: initConstraintWeight,
      optStatus: { tag: "NewIter" },
    },
    // pendingPaths: findPending(translation),
  };
  return evalShapes(sampledState);
};

//#endregion
