import { updateVaryingValues } from "engine/PropagateUpdate";
import { Properties, Shape } from "types/shape";
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
  const updated: State = {
    ...state,
    params: { ...state.params, optStatus: "NewIter" },
    shapes: state.shapes.map(({ shapeType, properties }: Shape) => {
      if (properties.name.contents === id) {
        return dragShape({ shapeType, properties }, [dx, dy]);
      }
      return { shapeType, properties };
    }),
  };
  // TODO: need to retrofit this implementation to the new State type
  const updatedWithVaryingState = updateVaryingValues(updated);
  return updatedWithVaryingState;
};

// TODO: factor out position props in shapedef
const dragShape = (shape: Shape, offset: [number, number]): Shape => {
  const { shapeType, properties } = shape;
  switch (shapeType) {
    case "Path":
      console.log("Path drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return shape;
    case "Polygon":
      console.log("Polygon drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return shape;
    case "Polyline":
      console.log("Polyline drag unimplemented", shape); // Just to prevent crashing on accidental drag
      return shape;
    case "Line":
      return {
        ...shape,
        properties: moveProperties(properties, ["start", "end"], offset),
      };
    case "Arrow":
      return {
        ...shape,
        properties: moveProperties(properties, ["start", "end"], offset),
      };
    default:
      return {
        ...shape,
        properties: moveProperties(properties, ["center"], offset),
      };
  }
};

/**
 * For each of the specified properties listed in `propPairs`, subtract a number from the original value.
 */
const moveProperties = (
  properties: Properties<number>,
  propsToMove: string[],
  [dx, dy]: [number, number]
): Properties<number> => {
  const moveProperty = (props: Properties<number>, propertyID: string) => {
    const [x, y] = props[propertyID].contents as [number, number];
    props[propertyID].contents = [x + dx, y + dy];
    return props;
  };
  return propsToMove.reduce(moveProperty, properties);
};
