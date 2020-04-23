import {
  decodeState,
  findExpr,
  evalTranslation,
  evalExpr,
  resolvePath,
} from "../Evaluator";
import * as stateJSON from "./state.json";

// TODO: there are type errors in this file, but `npm test` seems to run just fine, why?

// throwing away the flag
const state = decodeState(stateJSON.contents);

describe("state operations tests", () => {
  it("decodes an existing state and make sure nothing is undefined", () => {
    expect(Object.values(decodeState)).not.toContain(undefined);
  });

  it("finds a shape in the decoded state", () => {
    const trans = state.translation.trMap;
    expect(trans["A"]).not.toEqual(undefined);
  });

  it("finds all shape expressions in a state using shapePaths", () => {
    const shapes = state.shapePaths.map((p) => findExpr(state.translation, p));
    expect(shapes).not.toContain(undefined);
  });

  it("gets the value of the field `A.shape`", () => {
    const path: IFieldPath = {
      tag: "FieldPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shape"],
    };
    const shape = findExpr(state.translation, path);
    expect(shape).not.toEqual(undefined);
  });

  it("gets the value of the property `A.shape.x`", () => {
    const path: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shape", "x"],
    };
    const prop = findExpr(state.translation, path) as TagExpr<number>;
    expect(prop.contents.tag).toEqual("FloatV");
  });
});

describe("evaluation functions tests", () => {
  it("evaluates a single unary operation A.shape.strokeWidth", () => {
    const path: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shape", "strokeWidth"],
    };
    const prop = findExpr(state.translation, path) as IOptEval<number>;
    const propEvaled = evalExpr(
      prop.contents as Expr,
      state.translation,
      state.varyingMap
    ) as IVal<number>;
    expect(prop.contents.tag).toEqual("UOp");
    expect(propEvaled.contents.contents).toEqual(-0);
  });
  it("evaluates a single computation A.text.color", () => {
    const path: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "text", "color"],
    };
    const prop = findExpr(state.translation, path) as IOptEval<number>;
    expect(prop.contents.tag).toEqual("CompApp");
  });
  it("resolve a field path const.num", () => {
    const path: IFieldPath = {
      tag: "FieldPath",
      contents: [{ tag: "BStyVar", contents: "const" }, "num"],
    };
    // NOTE: not using varying values
    const propVal = resolvePath(path, state.translation, []).contents as Value<
      number
    >;
    expect(propVal.contents).toEqual(42);
  });
  it("resolve a property path A.shading.x", () => {
    const path1: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shading", "x"],
    };
    const path2: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shape", "x"],
    };
    // NOTE: not using varying values
    const propVal1: ArgVal<number> = resolvePath(path1, state.translation, []);
    const propVal2: TagExpr<number> = findExpr(
      state.translation,
      path2
    ) as TagExpr<number>;
    expect(propVal1.contents).toEqual(propVal2.contents);
  });
  it("resolve a property path A.shadow.w, which is computed via A.shape.r", () => {
    const path1: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shadow", "w"],
    };
    const path2: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shape", "r"],
    };
    // NOTE: not using varying values
    const propVal1: ArgVal<number> = resolvePath(path1, state.translation, []);
    const propVal2: TagExpr<number> = findExpr(
      state.translation,
      path2
    ) as IDone<number>;
    expect(propVal1.contents.contents).toEqual(
      propVal2.contents.contents * 2.15
    );
  });
  // it("evaluates the whole translation and output a list of fully evaluated shapes", () => {
  // evalTranslation(state);
  // });
});
