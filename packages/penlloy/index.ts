import * as fs from "fs";
import { ModelShape } from "./types/ModelShape.js";
import {
  ProcessDomainResult,
  processRel,
  processSig,
} from "./utils/ModelProcessor.js";
import { SigName } from "./types/DomainObject.js";

const sigsJson = fs.readFileSync("sigs.json", "utf8");

const modelShape = JSON.parse(sigsJson) as ModelShape;

const result: ProcessDomainResult = {
  sigTypes: [],
  conjTypes: [],
  subTypes: [],
  rels: [],
};

const addResult = (prev: ProcessDomainResult, curr: ProcessDomainResult) => {
  prev.sigTypes.push(...curr.sigTypes);
  prev.conjTypes.push(...curr.conjTypes);
  prev.subTypes.push(...curr.subTypes);
  prev.rels.push(...curr.rels);
};

for (const sig of modelShape.sigs) {
  const thisResult = processSig(sig);
  addResult(result, thisResult);
}

for (const rel of modelShape.rels) {
  const thisResult = processRel(rel);
  addResult(result, thisResult);
}

// Build domain

const prog: string[] = ["type Rel"];

const toDomainName = (name: SigName) => {
  if (name.tag === "SingleSigName") {
    return `_sig_${name.contents.replaceAll("/", "_SLASH_")}`;
  } else {
    return `_conj_${name.contents
      .map((n) => n.replaceAll("/", "_SLASH_"))
      .join("_OR_")}`;
  }
};

for (const sigType of result.sigTypes) {
  const line = `type ${toDomainName(sigType.contents)}`;
  if (!prog.includes(line)) {
    prog.push(line);
  }
}

for (const conjType of result.conjTypes) {
  const line = `type ${toDomainName(conjType.contents)}`;
  if (!prog.includes(line)) {
    prog.push(line);
  }
}

for (const subType of result.subTypes) {
  const line = `${toDomainName(subType.sub)} <: ${toDomainName(subType.sup)}`;
  if (!prog.includes(line)) {
    prog.push(line);
  }
}

for (const rel of result.rels) {
  const { belongsTo, name, argTypes } = rel;
  const line = `function _rel_${belongsTo.contents.replaceAll(
    "/",
    "_SLASH_",
  )}_${name} (${argTypes.map(toDomainName).join(", ")}) -> Rel`;
  if (!prog.includes(line)) {
    prog.push(line);
  }
}

fs.writeFileSync("out.domain", prog.join("\n"));
