import { exprToNumber } from "engine/EngineUtils";
import { A } from "types/ast";
import { Shape } from "types/shape";
import { State } from "types/state";
import { Path, PropertyPath } from "types/style";
import { Value } from "types/value";

/**
 * Find the value of a property in a list of fully evaluated shapes.
 *
 * @param shapes a list of shapes
 * @param path a path to a property value in one of the shapes
 */
// the `any` is to accomodate `collectLabels` storing updated property values in a new property that's not in the type system
const findShapeProperty = (shapes: any, path: Path<A>): Value<number> | any => {
  const getProperty = (path: PropertyPath<A>) => {
    const [subName, field, prop]: [string, string, string] = [
      path.name.contents.value,
      path.field.value,
      path.property.value,
    ];

    // HACK: this depends on the name encoding
    const shape = shapes.find((s: any) => {
      const [nameStr, fieldStr]: [string, string] = [subName, field];
      const currName: string = s.properties.name.contents;
      return currName === `${nameStr}.${fieldStr}`;
    });

    if (!shape) {
      console.log(
        "shapes",
        shapes,
        shapes.map((s: Shape) => s.properties.name.contents)
      );
      throw Error(`shape not found: ${subName}.${field}`);
    }

    return shape.properties[prop];
  };
  switch (path.tag) {
    case "FieldPath":
      throw new Error("pending paths must be property paths");
    case "PropertyPath": {
      return getProperty(path);
    }
    case "AccessPath": {
      const { path: propertyPath, indices } = path;
      if (propertyPath.tag === "PropertyPath") {
        const property = getProperty(propertyPath);
        // walk the structure to access all indices
        let res = property.contents;
        for (const i of indices) {
          res = res[exprToNumber(i)];
        }
        return res;
      } else {
        throw new Error(
          `pending paths must be property paths but got ${propertyPath.tag}`
        );
      }
    }
  }
};

/** Generate a single string based on a path to a shape */
export const getShapeName = (p: Path<A>): string => {
  if (p.tag === "FieldPath" || p.tag === "PropertyPath") {
    const { name, field } = p;
    return `${name.contents.value}.${field.value}`;
  } else {
    throw new Error("Can only derive shape name from field or property path.");
  }
};

/**
 * Take all pending paths in the state, find values for them from shapes in the state, insert these values in the translation, and finally clear pending paths.
 *
 * @param state initial state with pending values
 *
 */
export const insertPending = (state: State): State => {
  return {
    ...state,
    // clear up pending paths now that they are updated properly
    pendingPaths: [],
    // TODO: write the rest of this function
  };
};

/**
 * Back propagate value changes in shapes to varying values
 * @param state Old diagram state
 */
export const updateVaryingValues = (state: State): State => {
  const newVaryingValues = [...state.varyingValues];
  state.varyingPaths.forEach((path: Path<A>, index: number) => {
    // NOTE: We only update property paths since no frontend interactions can change fields
    // TODO: add a branch for `FieldPath` when this is no longer the case
    if (path.tag === "PropertyPath") {
      newVaryingValues[index] = findShapeProperty(state.shapes, path).contents;
    } else if (path.tag === "AccessPath" && path.path.tag === "PropertyPath") {
      newVaryingValues[index] = findShapeProperty(state.shapes, path);
    }
  });
  return {
    ...state,
    varyingValues: newVaryingValues,
  };
};
