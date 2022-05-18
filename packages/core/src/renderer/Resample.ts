import { makeADInputVars } from "engine/Autodiff";
import {
  compileCompGraph,
  initConstraintWeight,
  valueAutodiffToNumber,
} from "engine/EngineUtils";
import { evalShapes } from "engine/Evaluator";
import { mapValues } from "lodash";
import seedrandom from "seedrandom";
import { Canvas } from "shapes/Samplers";
import { ShapeDef, shapedefs } from "shapes/Shapes";
import { A } from "types/ast";
import { Shape } from "types/shape";
import { State } from "types/state";
import { Path } from "types/style";
import { TagExpr, Value } from "types/value";
import { prettyPrintPath, randFloat, randFloats, safe } from "utils/Util";

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
export const sampleShapes = (
  rng: seedrandom.prng,
  shapes: Shape[],
  canvas: Canvas
): Shape[] =>
  shapes.map((shape: Shape) =>
    sampleShape(rng, shape, shapedefs[shape.shapeType], canvas)
  );

/**
 * Resample all properties of one shape.
 *
 * @param shape existing shape
 * @param shapeDef shape definition
 * @ignore
 */
const sampleShape = (
  rng: seedrandom.prng,
  shape: Shape,
  shapeType: string,
  canvas: Canvas
): Shape => ({
  ...shape,
  properties: mapValues(shape.properties, (_: Value<number>, prop: string) =>
    sampleProperty(rng, prop, shapeType, canvas)
  ),
});

/**
 * Sample a property based on a shape definition.
 *
 * @param property property name
 * @param shapeDef shape definition
 */
const sampleProperty = (
  rng: seedrandom.prng,
  property: string,
  shapeType: string,
  canvas: Canvas
): Value<number> => {
  const shapedef: ShapeDef = shapedefs[shapeType];
  // TODO: don't resample all the properties every time for each property
  const props = shapedef.sampler(rng, canvas);
  if (property in props) {
    // TODO: don't make ad.Num only to immediately convert back to number
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
  rng: seedrandom.prng,
  { varyingPaths }: State,
  canvas: Canvas
): number[] => {
  const fieldPaths = varyingPaths.filter(
    ({ tag }: Path<A>) => tag === "AccessPath" || tag === "FieldPath"
  );
  return randFloats(rng, fieldPaths.length, canvas.xRange);
};

const samplePath = (
  rng: seedrandom.prng,
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
    return { tag: "FloatV", contents: randFloat(rng, ...canvas.xRange) };
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
    const sampledProp: Value<number> = sampleProperty(
      rng,
      prop,
      shapeType,
      canvas
    );
    return sampledProp;
  }
};

export const resampleOnce = (rng: seedrandom.prng, state: State): State => {
  // resample all the uninitialized and varying values
  const { varyingPaths, shapes, params } = state;
  const varyingValues: Value<number>[] = varyingPaths.map((p: Path<A>) =>
    samplePath(rng, p, shapes, state.varyingInitInfo, state.canvas)
  );

  const sampledState: State = {
    ...state,
    varyingValues: varyingValues.map((v) => val2float(v)),
    params: {
      ...params,
      weight: initConstraintWeight,
      optStatus: "NewIter" as const,
    },
    // pendingPaths: findPending(translation),
  };

  // re-compile comp graph
  const varyingVars = makeADInputVars(sampledState.varyingValues);
  const computeShapes = compileCompGraph(
    evalShapes(rng, sampledState, varyingVars)
  );

  // NOTE: we don't re-generate the optimization graph because all floating-point values that could've changed by re-sampling are automatically included in the varying values. Therefore, coincidentally, the opt graph and resampled values can never go out of sync. Also, because non-float values do not participate in optimization at all, they don't cause any problems.

  return {
    ...sampledState,
    computeShapes,
    shapes: computeShapes(sampledState.varyingValues),
  };
};
