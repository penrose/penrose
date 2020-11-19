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

type Sampler = (() => Value<number>) | ConstSampler;

type ConstSampler = (type: PropType, value: PropContents) => Value<number>;

const vectorSampler: Sampler = () => ({
  tag: "VectorV",
  contents: [randFloat(...canvasSize), randFloat(...canvasSize)],
});
const widthSampler: Sampler = () => ({
  tag: "FloatV",
  contents: randFloat(3, canvasSize[0] / 6),
});
const heightSampler: Sampler = () => ({
  tag: "FloatV",
  contents: randFloat(3, canvasSize[1] / 6),
});
const strokeSampler: Sampler = () => ({
  tag: "FloatV",
  contents: randFloat(0.5, 3),
});

const constValue: ConstSampler = (tag: string, contents: PropContents) => ({
  tag,
  contents,
});

export const circleType: ShapeDef = {
  shapeType: "Circle",
  properties: {
    center: ["VectorV", vectorSampler],
    r: ["FloatV", widthSampler],
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue()],
  },

  // , ("strokeStyle", (StrT, constValue $ StrV "solid"))
  // , ("strokeColor", (ColorT, sampleColor))
  // , ("color", (ColorT, sampleColor))
  // , ("name", (StrT, constValue $ StrV "defaultCircle"))
};
