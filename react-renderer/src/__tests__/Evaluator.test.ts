import React from "react";
import { render, unmountComponentAtNode } from "react-dom";
import { act } from "react-dom/test-utils";
import { decodeState, findExpr, evalTranslation } from "../Evaluator";
import { PropertyPath } from "lodash";

const stateJSON = require("./state.json");
// throwing away the flag
const state = decodeState(stateJSON.contents);

describe("state operations tests", () => {
  it("decodes an existing state and make sure nothing is undefined", () => {
    expect(Object.values(decodeState)).not.toContain(undefined);
  });

  it("finds a shape in the decoded state", () => {
    const name1 = state.translation.trMap.keys().next().value;
    expect(state.translation.trMap.get("A")).not.toEqual(undefined);
    expect(state.translation.trMap.get(name1)).not.toEqual(undefined);
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
    console.log("Found A.shape", shape);
    expect(shape).not.toEqual(undefined);
  });

  it("gets the value of the property `A.shape.x`", () => {
    const path: IPropertyPath = {
      tag: "PropertyPath",
      contents: [{ tag: "BSubVar", contents: "A" }, "shape", "x"],
    };
    const prop = findExpr(state.translation, path);
    console.log("Found A.shape.x", prop);
    expect(prop.contents.tag).toEqual("FloatV");
  });
});

describe("evaluation functions tests", () => {
  it("evaluates the whole translation and output a list of fully evaluated shapes", () => {
    evalTranslation(state);
  });
});
