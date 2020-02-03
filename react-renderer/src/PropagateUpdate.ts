// helper for finding a shape by name
const findShapeProperty = (shapes: any, name: string, property: string) =>
  shapes.find((shape: any) => shape[1].name.contents === name)[1][property];

// helper for updating a pending property given a path
const updateProperty = (translation: any, shapes: any, path: any) => {
  const [subName, fieldName, propertyName] = path.contents;
  if (path.tag === "PropertyPath") {
    return {
      ...translation,
      trMap: translation.trMap.map(([sub, fieldDict]: [any, any]) => {
        // match substance name
        if (sub.contents === subName.contents) {
          // TODO: functional-style map on objects doesn't seem to be supported by TS well. Write helpfer?
          const updatedFieldDict = { ...fieldDict };
          for (const field of Object.keys(fieldDict)) {
            // match field name
            if (field === fieldName) {
              const {
                contents: [, propertyDict]
              } = updatedFieldDict[field];
              // shape name is a done value of type string, hence the two accesses
              const shapeName = propertyDict.name.contents.contents;
              // find property and updated value
              const propWithUpdate = findShapeProperty(
                shapes,
                shapeName,
                propertyName
              );
              // update the property in the shape list
              propWithUpdate.contents = propWithUpdate.updated;
              const { tag, contents } = propWithUpdate;
              delete propWithUpdate.updated;

              // update the pending property in the translated by a Done value retrieved from the shapes, which are already updated (in the two lines above)
              propertyDict[propertyName] = {
                tag: "Done",
                contents: { tag, contents }
              };
            }
          }
          return [sub, updatedFieldDict];
        } else {
          return [sub, fieldDict];
        }
      })
    };
  } else {
    console.error("Pending field paths are not supported");
  }
};

export const propagateUpdate = (data: any) => {
  return {
    ...data,
    // clear up pending paths now that they are updated properly
    pendingPaths: [],
    // for each of the pending path, update the translation using the updated shapes with new label dimensions etc.
    transr: data.pendingPaths.reduce(
      (trans: any, path: any) =>
        updateProperty(data.transr, data.shapesr, path),
      data.transr
    )
  };
};
export const updateVaryingState = async (data: any) => {
  const newVaryingState = [...data.varyingState];
  await data.varyingPaths.forEach((path: any, index: number) => {
    // NOTE: We only update property paths since no frontend interactions can change fields
    // TODO: add a branch for `FieldPath` when this is no longer the case
    if (path.tag === "PropertyPath") {
      const [{ contents: subName }, fieldName, propertyName] = path.contents;
      data.transr.trMap.forEach(
        ([subVar, fieldDict]: [any, any], fieldIndex: number) => {
          if (subVar.contents === subName) {
            const propertyDict = fieldDict[fieldName].contents[1];
            const shapeName = propertyDict.name.contents.contents;
            newVaryingState[index] = findShapeProperty(
              data.shapesr,
              shapeName,
              propertyName
            ).contents;
          }
        }
      );
    }
  });
  return {
    ...data,
    varyingState: newVaryingState
  };
};
