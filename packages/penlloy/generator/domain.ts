import {
  DomainConjunctionType,
  DomainRel,
  DomainSigType,
  DomainSubType,
  SigName,
  SingleSigName,
} from "../types/DomainObject.js";
import { ModelRel, RawAlloyModel, ModelSig } from "../types/RawAlloyModel.js";

export type CompiledModel = {
  sigTypes: DomainSigType[];
  conjTypes: DomainConjunctionType[];
  subTypes: DomainSubType[];
  rels: DomainRel[];
};

export const compileModel = (shape: RawAlloyModel): CompiledModel => {
  const initRes: CompiledModel = {
    sigTypes: [],
    conjTypes: [],
    subTypes: [],
    rels: [],
  };

  const addResult = (prev: CompiledModel, curr: CompiledModel) => {
    return {
      sigTypes: [...prev.sigTypes, ...curr.sigTypes],
      conjTypes: [...prev.conjTypes, ...curr.conjTypes],
      subTypes: [...prev.subTypes, ...curr.subTypes],
      rels: [...prev.rels, ...curr.rels],
    };
  };

  const afterSigs = shape.sigs.reduce(
    (acc, sig) => addResult(acc, processSig(sig)),
    initRes,
  );

  const afterRels = shape.rels.reduce(
    (acc, rel) => addResult(acc, processRel(rel)),
    afterSigs,
  );

  return afterRels;
};

const toDomainName = (name: SigName) => {
  if (name.tag === "SingleSigName") {
    return `_sig_${name.contents.replaceAll("/", "_SLASH_")}`;
  } else {
    return `_conj_${name.contents
      .map((n) => n.replaceAll("/", "_SLASH_"))
      .join("_OR_")}`;
  }
};

export const translateToDomain = ({
  sigTypes,
  conjTypes,
  subTypes,
  rels,
}: CompiledModel): string => {
  const prog: string[] = ["type Rel"];

  for (const sigType of sigTypes) {
    const line = `type ${toDomainName(sigType.contents)}`;
    if (!prog.includes(line)) {
      prog.push(line);
    }
  }

  for (const conjType of conjTypes) {
    const line = `type ${toDomainName(conjType.contents)}`;
    if (!prog.includes(line)) {
      prog.push(line);
    }
  }

  for (const subType of subTypes) {
    const line = `${toDomainName(subType.sub)} <: ${toDomainName(subType.sup)}`;
    if (!prog.includes(line)) {
      prog.push(line);
    }
  }

  for (const rel of rels) {
    const { belongsTo, name, argTypes } = rel;
    const line = `function _rel_${belongsTo.contents.replaceAll(
      "/",
      "_SLASH_",
    )}_${name} (${argTypes.map(toDomainName).join(", ")}) -> Rel`;
    if (!prog.includes(line)) {
      prog.push(line);
    }
  }

  return prog.join("\n");
};

const processSigName = (raw: string): SingleSigName => {
  return {
    tag: "SingleSigName",
    contents: raw,
  };
};

const processSig = (sig: ModelSig): CompiledModel => {
  const { name, parent, supersets } = sig;

  const result: CompiledModel = {
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

const processRel = (rel: ModelRel): CompiledModel => {
  const { name, type, sig } = rel;
  const sigName = processSigName(sig);

  const result: CompiledModel = {
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
