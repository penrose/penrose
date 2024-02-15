import {
  DomainConjunctionType,
  DomainRel,
  DomainSigType,
  DomainSubType,
  SigName,
  SingleSigName,
} from "../types/DomainObject.js";
import { ModelRel, ModelSig } from "../types/ModelShape.js";

export type ProcessDomainResult = {
  sigTypes: DomainSigType[];
  conjTypes: DomainConjunctionType[];
  subTypes: DomainSubType[];
  rels: DomainRel[];
};

export const processSigName = (raw: string): SingleSigName => {
  return {
    tag: "SingleSigName",
    contents: raw,
  };
};

export const processSig = (sig: ModelSig): ProcessDomainResult => {
  const { name, parent, supersets } = sig;

  const result: ProcessDomainResult = {
    sigTypes: [],
    conjTypes: [],
    subTypes: [],
    rels: [],
  };

  const processedName = processSigName(name);

  result.sigTypes.push({
    tag: "DomainSigType",
    contents: processedName,
  });

  if (parent !== undefined) {
    const processedParentName = processSigName(parent);
    result.subTypes.push({
      tag: "DomainSubType",
      sub: processedName,
      sup: processedParentName,
    });
  }

  if (supersets !== undefined) {
    for (const superset of supersets) {
      const processedSupersetName = processSigName(superset);
      result.subTypes.push({
        tag: "DomainSubType",
        sub: processedName,
        sup: processedSupersetName,
      });
    }
  }

  return result;
};

export const processRel = (rel: ModelRel): ProcessDomainResult => {
  const { name, type, sig } = rel;
  const sigName = processSigName(sig);

  const result: ProcessDomainResult = {
    sigTypes: [],
    conjTypes: [],
    subTypes: [],
    rels: [],
  };

  const realType = processRelType(type);

  for (const t of realType) {
    if (t.tag === "SingleSigName") {
      result.sigTypes.push({
        tag: "DomainSigType",
        contents: t,
      });
    } else {
      for (const singleType of t.contents) {
        result.subTypes.push({
          tag: "DomainSubType",
          sub: {
            tag: "SingleSigName",
            contents: singleType,
          },
          sup: t,
        });
      }
      result.conjTypes.push({
        tag: "DomainConjunctionType",
        contents: t,
      });
    }
  }

  result.rels.push({
    tag: "DomainRel",
    belongsTo: sigName,
    name,
    argTypes: realType,
  });

  return result;
};

type ProductType = SingleSigName[];

const processProductType = (raw: string): ProductType =>
  raw.split("->").map(processSigName);

/**
 * Construct a conjunction type out of the provided sig names
 * @param names
 */
const makeConjType = (names: SingleSigName[]): SigName => {
  const ns = new Set(names.map((n) => n.contents));
  const sorted = Array.from(ns).sort();
  if (sorted.length === 1) {
    return {
      tag: "SingleSigName",
      contents: sorted[0],
    };
  } else {
    return {
      tag: "ConjunctionSigName",
      contents: sorted,
    };
  }
};

const processRelType = (raw: string) => {
  const products = raw
    .substring(1, raw.length - 1)
    .split(", ")
    .map(processProductType);

  // sanity checks
  // 1. all arities are equal
  if (new Set(products.map((p) => p.length)).size !== 1) {
    throw new Error(`Type ${raw} has elements of different arities`);
  }

  const transposed = products[0].map((_, colIndex) =>
    products.map((row) => row[colIndex]),
  );

  return transposed.map(makeConjType);
};
