import {
  valueNumberToAutodiff,
  tagExprNumberToAutodiff,
  insertExpr,
  initConstraintWeight,
  shapeAutodiffToNumber,
  valueAutodiffToNumber,
} from "engine/EngineUtils";
import { evalShapes } from "engine/Evaluator";
import { Shape } from "types/shape";
import { Value } from "types/value";
import { randFloat, randFloats, safe, prettyPrintPath } from "utils/Util";
import { mapValues } from "lodash";
import { TagExpr, Translation } from "types/value";
import { VarAD } from "types/ad";
import { State } from "types/state";
import { Path } from "types/style";
import { Canvas } from "shapes/Samplers";
import { shapedefs } from "shapes/Shapes";
import { A } from "types/ast";

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
 *
 * @param shapes Old shapes
 * @ignore
 */
export const sampleShapes = (shapes: Shape[], canvas: Canvas): Shape[] =>
  shapes.map((shape: Shape) =>
    sampleShape(shape, shapedefs[shape.shapeType], canvas)
  );

/**
 * Resample all properties of one shape.
 *
 * @param shape existing shape
 * @param shapeDef shape definition
 * @ignore
 */
const sampleShape = (
  shape: Shape,
  shapeType: string,
  canvas: Canvas
): Shape => ({
  ...shape,
  properties: mapValues(shape.properties, (_: Value<number>, prop: string) =>
    sampleProperty(prop, shapeType, canvas)
  ),
});

/**
 * Sample a property based on a shape definition.
 *
 * @param property property name
 * @param shapeDef shape definition
 */
const sampleProperty = (
  property: string,
  shapeType: string,
  canvas: Canvas
): Value<number> => {
  // TODO: don't resample all the properties every time for each property
  const props = shapedefs[shapeType].sampler(canvas);
  if (property in props) {
    // TODO: don't make VarAD only to immediately convert back to number
    return valueAutodiffToNumber(props[property]);
  } else {
    throw new Error(
      `${property} is not a valid property to be sampled for shape ${shapeType}.`
    );
  }
};

/**
 * Sample varying fields, which are assumed to be positional values. They are sampled with the canvas dimensions.
 *
 * @param state State that contains a list of varying paths
 * @ignore
 */
export const sampleFields = (
  { varyingPaths }: State,
  canvas: Canvas
): number[] => {
  const fieldPaths = varyingPaths.filter(
    ({ tag }: Path<A>) => tag === "AccessPath" || tag === "FieldPath"
  );
  return randFloats(fieldPaths.length, canvas.xRange);
};

const samplePath = (
  path: Path<A>,
  shapes: Shape[],
  varyingInitInfo: { [pathStr: string]: number },
  canvas: Canvas
): Value<number> => {
  if (path.tag === "LocalVar" || path.tag === "InternalLocalVar") {
    throw Error("local path shouldn't appear in GPI");
  }

  const pathStr = prettyPrintPath(path);
  if (pathStr in varyingInitInfo) {
    return {
      tag: "FloatV",
      contents: varyingInitInfo[pathStr],
    };
  }

  // HACK: for access and field paths, sample within the canvas width
  if (path.tag === "AccessPath" || path.tag === "FieldPath") {
    return { tag: "FloatV", contents: randFloat(...canvas.xRange) };
  }
  // for property path, use the sampler in shapedef
  else {
    const [subName, field, prop]: [string, string, string] = [
      path.name.contents.value,
      path.field.value,
      path.property.value,
    ];
    const { shapeType } = safe(
      shapes.find(
        (s: any) => s.properties.name.contents === `${subName}.${field}`
      ),
      `Cannot find shape ${subName}.${field}`
    );
    const sampledProp: Value<number> = sampleProperty(prop, shapeType, canvas);
    return sampledProp;
  }
};

export const resampleBest = (state: State, numSamples: number): State => {
  // resample all the uninitialized and varying values
  const { varyingPaths, shapes, uninitializedPaths, params } = state;
  const varyingValues: Value<number>[] = varyingPaths.map((p: Path<A>) =>
    samplePath(p, shapes, state.varyingInitInfo, state.canvas)
  );

  // update the translation with all uninitialized values (converted to `Done` values)
  const uninitMap: [
    Path<A>,
    TagExpr<VarAD>
  ][] = uninitializedPaths.map((p: Path<A>) => [
    p,
    val2Expr(
      valueNumberToAutodiff(
        samplePath(p, shapes, state.varyingInitInfo, state.canvas)
      )
    ),
  ]);

  const translation: Translation = uninitMap.reduce(
    (tr: Translation, [p, e]: [Path<A>, TagExpr<VarAD>]) =>
      insertExpr(p, e, tr),
    state.translation
  );

  const sampledState: State = {
    ...state,
    varyingValues: varyingValues.map((v) => val2float(v)),
    translation,
    params: {
      ...params,
      weight: initConstraintWeight,
      optStatus: "NewIter" as const,
    },
    // pendingPaths: findPending(translation),
  };
  return {
    ...sampledState,
    shapes: shapeAutodiffToNumber(evalShapes(sampledState)),
  };
};
