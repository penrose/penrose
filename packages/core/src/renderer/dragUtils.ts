import { genOptProblem } from "engine/Optimizer";
import * as ad from "types/ad";
import { Properties, ShapeAD } from "types/shape";
import { State } from "types/state";

/**
 * Retrieve data from drag events and update varying state accordingly
 */
export const dragUpdate = (
  state: State,
  id: string,
  dx: number,
  dy: number
): State => {
  const xs = [...state.varyingValues];
  for (const shape of state.shapes) {
    if (shape.properties.name.contents === id) {
      dragShape(shape, [dx, dy], xs);
    }
  }
  const { inputs, constraintSets } = state;
  const updated: State = {
    ...state,
    params: genOptProblem(inputs, constraintSets, "ShapeLayout", [
      "LabelLayout",
      "Overall",
    ]),
    varyingValues: xs,
  };
  return updated;
};

// TODO: factor out position props in shapedef
const dragShape = (
  shape: ShapeAD,
  offset: [number, number],
  xs: number[]
): void => {
  const { shapeType, properties } = shape;
  switch (shapeType) {
    case "Path":
      console.log("Path drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return;
    case "Polygon":
      console.log("Polygon drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return;
    case "Polyline":
      console.log("Polyline drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return;
    case "Line":
      moveProperties(properties, ["start", "end"], offset, xs);
      return;
    default:
      moveProperties(properties, ["center"], offset, xs);
      return;
  }
};

/**
 * For each of the specified properties listed in `propPairs`, subtract a number from the original value.
 */
const moveProperties = (
  properties: Properties<ad.Num>,
  propsToMove: string[],
  [dx, dy]: [number, number],
  xs: number[]
): void => {
  for (const propertyID of propsToMove) {
    const value = properties[propertyID];
    if (value.tag === "VectorV") {
      const [x, y] = value.contents;
      if (typeof x !== "number" && x.tag === "Input") {
        xs[x.key] += dx;
      }
      if (typeof y !== "number" && y.tag === "Input") {
        xs[y.key] += dy;
      }
    }
  }
};
