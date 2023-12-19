import { Env, PenroseError, Result, showError } from "@penrose/core";
import { LabelCache, State } from "@penrose/core/dist/types/state";
import { SubstanceEnv } from "@penrose/core/dist/types/substance";
import {
  collectLabels,
  insertPending,
  mathjaxInit,
} from "@penrose/core/dist/utils/CollectLabels";

import { compileStyle as rawCompileStyle } from "@penrose/core/dist/compiler/Style";

export const computeLabels = async (state: State) => {
  const convert = mathjaxInit();
  const labelCache: Result<LabelCache, PenroseError> = await collectLabels(
    state.shapes,
    convert,
  );

  if (labelCache.isErr()) {
    throw new Error(`Failed to compute labels: ${showError(labelCache.error)}`);
  }
  return insertPending({ ...state, labelCache: labelCache.value });
};

export const compileStyle = async (
  variation: string,
  stySource: string,
  excludeWarnings: string[],
  subEnv: SubstanceEnv,
  varEnv: Env,
) => {
  const raw = await rawCompileStyle(
    variation,
    stySource,
    excludeWarnings,
    subEnv,
    varEnv,
  );
  if (raw.isErr()) {
    throw new Error(`Failed to compile style: ${showError(raw.error)}`);
  }
  return computeLabels(raw.value);
};
