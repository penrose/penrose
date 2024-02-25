export const toPenroseTypeName = (sigName: string) => {
  return `_sig_${sigName.replaceAll("/", "_SLASH_")}`;
};

export const toPenroseRelationName = (belongsTo: string, relName: string) => {
  return `_rel_${belongsTo.replaceAll("/", "_SLASH_")}_${relName}`;
};

export const toPenroseObjectName = (atomName: string) => {
  return `_obj_${atomName.replaceAll("/", "_SLASH_").replaceAll("$", "_")}`;
};
