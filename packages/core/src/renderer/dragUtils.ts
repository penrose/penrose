import * as ad from "../types/ad";
import { Properties, ShapeAD } from "../types/shape";
import { State } from "../types/state";

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
  const frozenValues = [];
  for (const shape of state.shapes) {
    if (shape.properties.name.contents === id) {
      const ids = dragShape(shape, [dx, dy], xs);
      frozenValues.push(...ids);
    }
  }
  const updated: State = {
    ...state,
    params: {
      ...state.params,
      optStatus: "NewIter",
    },
    varyingValues: xs,
    frozenValues,
  };
  return updated;
};

// TODO: factor out position props in shapedef
// return: a list of updated ids
const dragShape = (
  shape: ShapeAD,
  offset: [number, number],
  xs: number[]
): number[] => {
  const { shapeType, properties } = shape;
  switch (shapeType) {
    case "Path":
      console.log("Path drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return [];
    case "Polygon":
      console.log("Polygon drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return [];
    case "Polyline":
      console.log("Polyline drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return [];
    case "Line":
      return moveProperties(properties, ["start", "end"], offset, xs);
    default:
      return moveProperties(properties, ["center"], offset, xs);
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
): number[] => {
  const ids: number[] = [];
  for (const propertyID of propsToMove) {
    const value = properties[propertyID];
    if (value.tag === "VectorV") {
      const [x, y] = value.contents;
      if (typeof x !== "number" && x.tag === "Input") {
        xs[x.key] += dx;
        ids.push(x.key);
      }
      if (typeof y !== "number" && y.tag === "Input") {
        xs[y.key] += dy;
        ids.push(y.key);
      }
    }
  }
  return ids;
};
