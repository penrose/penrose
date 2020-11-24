import { map, mapKeys, mapValues } from "lodash";
import { canvasSize } from "ui/Canvas";
import { randFloat } from "utils/Util";

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
    color: ["ColorV", colorSampler],
    name: ["StrV", () => constValue("StrV", "defaultText")],
    string: ["StrV", () => constValue("StrV", "defaultLabelText")],
    // HACK: typechecking is not passing due to Value mismatch. Not sure why
    polygon: ["PolygonV", () => constValue("PolygonV", emptyPoly as any)],
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

export const sampleShapes = (shapes: Shape[]): Shape[] =>
  shapes.map((shape: Shape) => sampleShape(shape, findDef(shape.shapeType)));

const sampleShape = (shape: Shape, shapeDef: ShapeDef): Shape => {
  const propModels: IPropModel = shapeDef.properties;
  const sampleProp = (propName: string): Value<number> => {
    const sampler = propModels[propName];
    if (sampler) return sampler[1]();
    else
      throw new Error(
        `${propName} is not a valid property to be sampled for shape ${shape.shapeType}.`
      );
  };
  return {
    ...shape,
    properties: mapValues(
      shape.properties,
      (_: Value<number>, propName: string) => sampleProp(propName)
    ),
  };
};

/**
 * Sample varying fields, which are assumed to be positional values. They are sampled with the canvas dimensions.
 */
// const sampleFields = () => {};
