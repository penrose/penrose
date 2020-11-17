import { insertExpr } from "engine/Evaluator";
import { valueNumberToAutodiff } from "engine/EngineUtils";

/**
 * Find the value of a property in a list of fully evaluated shapes.
 * @param shapes a list of shapes
 * @param path a path to a property value in one of the shapes
 */
// the `any` is to accomodate `collectLabels` storing updated property values in a new property that's not in the type system
const findShapeProperty = (shapes: any, path: Path): Value<number> | any => {
  const getProperty = (path: IPropertyPath) => {
    const [
      { contents: subName },
      field,
      prop,
    ] = (path as IPropertyPath).contents;
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
        throw new Error("pending paths must be property paths");
      }
    }
  }
};

/**
 * Take all pending paths in the state, find values for them from shapes in the state, insert these values in the translation, and finally clear pending paths.
 * @param state initial state with pending values
 *
 * @todo state shouldn't have the type `State` yet because the shapes are augumented with extra properties.
 * @todo: test with an initial state that has pending values
 */
export const insertPending = (state: State) => {
  return {
    ...state,
    // clear up pending paths now that they are updated properly
    pendingPaths: [],
    // for each of the pending paths, update the translation using the updated shapes with new label dimensions etc.
    translation: state.pendingPaths
      .map((p: Path) => [p, findShapeProperty(state.shapes, p).updated])
      // .updated is from `collectLabels` updating pending properties.
      // TODO: `v` can only be any for now due to `updated`
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
    } else if (path.tag === "AccessPath") {
      newVaryingValues[index] = findShapeProperty(state.shapes, path);
    }
  });
  return {
    ...state,
    varyingValues: newVaryingValues,
  };
};
