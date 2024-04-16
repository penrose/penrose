import { atom, selector, selectorFamily } from "recoil";

import {
  DomainEnv,
  PenroseError,
  PenroseWarning,
  State,
  compileDomain,
} from "@penrose/core";
import * as im from "immutable";
import { RefObject } from "react";
import { generateVariation } from "./variation";

export type ProgramType = "substance" | "domain" | "style";
export type ProgramContent = SubstanceProgram | DomainProgram | StyleProgram;

export type SubstanceProgram = string;
export type DomainProgram = string;
export type StyleProgram = string;

export const currentSubstanceProgramState = atom<SubstanceProgram>({
  key: "currentSubstanceProgramState",
  default: "",
});

export const currentDomainProgramState = atom<DomainProgram>({
  key: "currentDomainProgramState",
  default: "",
});

export const currentStyleProgramState = atom<StyleProgram>({
  key: "currentStyleProgramState",
  default: "",
});

const currentProgramStates = {
  style: currentStyleProgramState,
  substance: currentSubstanceProgramState,
  domain: currentDomainProgramState,
};

export const currentProgramSelector = selectorFamily<
  ProgramContent,
  ProgramType
>({
  key: "currentProgramSelector",
  get:
    (programType: ProgramType) =>
    ({ get }) => {
      return get(currentProgramStates[programType]);
    },
  set:
    (programType: ProgramType) =>
    ({ set }, newProgContent) => {
      set(currentProgramStates[programType], newProgContent);
    },
});

export const currentDirtyStyleProgramState = atom<StyleProgram>({
  key: "currentDirtyStyleProgramState",
  default: "",
});

export const currentDomainCacheSelector = selector<DomainEnv | null>({
  key: "currentDomainCacheSelector",
  get: ({ get }) => {
    const domainProgram = get(currentDomainProgramState);
    const env = compileDomain(domainProgram);
    if (env.isOk()) {
      return env.value;
    } else {
      return null;
    }
  },
});

export type StyleSVGResource = {
  contents: string;
};

export type StyleResources = im.Map<string, StyleSVGResource>;

export const currentStyleResourcesState = atom<StyleResources>({
  key: "currentStyleResourcesState",
  default: im.Map(),
});

export type DiagramMetadata = {
  variation: string;
  stepSize: number;
  autostep: boolean;
  interactive: boolean;
  excludeWarnings: string[];
  source: {
    domain: string;
    substance: string;
    style: string;
  };
};

export type Diagram = {
  state: State | null;
  error: PenroseError | null;
  warnings: PenroseWarning[];
  metadata: DiagramMetadata;
};

export const currentDiagramState = atom<Diagram>({
  key: "currentDiagramState",
  default: {
    state: null,
    error: null,
    warnings: [],
    metadata: {
      variation: generateVariation(),
      stepSize: 10000,
      autostep: true,
      interactive: false,
      excludeWarnings: [],
      source: {
        substance: "",
        style: "",
        domain: "",
      },
    },
  },
});

export type Canvas = {
  ref: RefObject<HTMLDivElement> | null;
};

export const currentCanvasState = atom<Canvas>({
  key: "currentCanvasState",
  default: { ref: null },
});
