import { insertExpr } from "engine/Evaluator";
import { valueNumberToAutodiff } from "engine/EngineUtils";
import { getShapeName } from "shapes/ShapeDef";
import { retrieveLabel } from "utils/CollectLabels";
import Label from "shapes/Label";

/**
 * Find the value of a property in a list of fully evaluated shapes.
 *
 * @param shapes a list of shapes
 * @param path a path to a property value in one of the shapes
 */
// the `any` is to accomodate `collectLabels` storing updated property values in a new property that's not in the type system
const findShapeProperty = (shapes: any, path: Path): Value<number> | any => {
  const getProperty = (p: IPropertyPath) => {
    const [{ contents: subName }, field, prop] = (p as IPropertyPath).contents;
    // HACK: this depends on the name encoding
    const shape = shapes.find(
      (s: any) => s.properties.name.contents === `${subName}.${field}`
    );
    return shape.properties[prop];
  };
  switch (path.tag) {
    case "FieldPath":
      throw new Error("pending paths must be property paths");
    case "PropertyPath": {
      return getProperty(path);
    }
    case "AccessPath": {
      const [propertyPath, indices] = (path as IAccessPath).contents;
      if (propertyPath.tag === "PropertyPath") {
        const property = getProperty(propertyPath);
        // walk the structure to access all indices
        let res = property.contents;
        for (let i of indices) {
          res = res[i];
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
export const insertPending = (state: State): State => {
  const findLabelValue = (p: Path, labels: LabelCache): Value<number> => {
    if (p.tag === "PropertyPath") {
      const [{ contents: subName }, field, prop] = p.contents;
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
        // TODO: types?
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
    } else if (
      path.tag === "AccessPath" &&
      path.contents[0].tag === "PropertyPath"
    ) {
      newVaryingValues[index] = findShapeProperty(state.shapes, path);
    }
  });
  return {
    ...state,
    varyingValues: newVaryingValues,
  };
};
