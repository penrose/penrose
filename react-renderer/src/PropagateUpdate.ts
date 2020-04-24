import { insertExpr } from "./Evaluator";

// helper for finding a shape by name
const findShapeProperty = (shapes: any, path: Path): Value<number> => {
  if (path.tag === "FieldPath") {
    throw new Error("pending paths must be property paths");
  } else {
    const [
      { contents: subName },
      field,
      prop,
    ] = (path as IPropertyPath).contents;
    const shape = shapes.find(
      (s: any) => s.properties.name.contents === `${subName}.${field}`
    );
    return shape.properties[prop];
  }
};

export const propagateUpdate = (state: State) => {
  return {
    ...state,
    // clear up pending paths now that they are updated properly
    pendingPaths: [],
    // for each of the pending path, update the translation using the updated shapes with new label dimensions etc.
    translation: state.pendingPaths
      .map((p: Path) => [p, findShapeProperty(state.shapes, p)])
      .reduce(
        (trans: Translation, [path, v]: [Path, Value<number>]) =>
          insertExpr(path, { tag: "Done", contents: v }, trans),
        state.translation
      ),
  };
};

// export const updateVaryingState = async (data: any) => {
//   const newVaryingState = [...data.varyingState];
//   await data.varyingPaths.forEach((path: any, index: number) => {
//     // NOTE: We only update property paths since no frontend interactions can change fields
//     // TODO: add a branch for `FieldPath` when this is no longer the case
//     if (path.tag === "PropertyPath") {
//       const [{ contents: subName }, fieldName, propertyName] = path.contents;
//       data.transr.trMap.forEach(
//         ([subVar, fieldDict]: [any, any], fieldIndex: number) => {
//           if (subVar.contents === subName) {
//             const propertyDict = fieldDict[fieldName].contents[1];
//             const shapeName = propertyDict.name.contents.contents;
//             newVaryingState[index] = findShapeProperty(
//               data.shapesr,
//               path
//             ).contents;
//           }
//         }
//       );
//     }
//   });
//   return {
//     ...data,
//     varyingState: newVaryingState,
//   };
// };
