import {
  valueNumberToAutodiff,
  insertExpr,
  exprToNumber,
} from "engine/EngineUtils";
import { getShapeName } from "renderer/ShapeDef";
import { Shape, Value } from "types/shapeTypes";
import { retrieveLabel } from "utils/CollectLabels";

/**
 * Find the value of a property in a list of fully evaluated shapes.
 *
 * @param shapes a list of shapes
 * @param path a path to a property value in one of the shapes
 */
// the `any` is to accomodate `collectLabels` storing updated property values in a new property that's not in the type system
const findShapeProperty = (shapes: any, path: Path): Value<number> | any => {
  const getProperty = (path: IPropertyPath) => {
    const [subName, field, prop]: [string, string, string] = [
      (path as IPropertyPath).name.contents.value,
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
      const [propertyPath, indices] = [
        (path as IAccessPath).path,
        path.indices,
      ];
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

/**
 * Take all pending paths in the state, find values for them from shapes in the state, insert these values in the translation, and finally clear pending paths.
 *
 * @param state initial state with pending values
 *
 * @todo state shouldn't have the type `State` yet because the shapes are augumented with extra properties.
 */
export const insertPending = (state: State) => {
  const findLabelValue = (p: Path, labels: LabelCache): Value<number> => {
    if (p.tag === "PropertyPath") {
      const { property } = p;
      const prop: string = property.value;
      const labelData = retrieveLabel(getShapeName(p), labels);
      if (labelData) {
        if (prop === "w") return labelData.w;
        else if (prop === "h") return labelData.h;
        else {
          throw new Error(`Cached label data do not contain property ${prop}`);
        }
      } else {
        throw new Error(`Label data not found for ${getShapeName(p)}`);
      }
    } else {
      throw new Error(
        `Pending value must be a property of a shape. Got a ${p.tag} instead.`
      );
    }
  };

  return {
    ...state,
    // clear up pending paths now that they are updated properly
    pendingPaths: [],
    // for each of the pending paths, update the translation using the updated shapes with new label dimensions etc.
    translation: state.pendingPaths
      .map((p: Path) => [p, findLabelValue(p, state.labelCache)])
      .reduce(
        (trans: Translation, [path, v]: any) =>
          insertExpr(
            path,
            { tag: "Done", contents: valueNumberToAutodiff(v) },
            trans
          ),
        state.translation
      ),
  };
};

/**
 * Back propagate value changes in shapes to varying values
 * @param state Old diagram state
 */
export const updateVaryingValues = (state: State): State => {
  const newVaryingValues = [...state.varyingValues];
  state.varyingPaths.forEach((path: Path, index: number) => {
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
