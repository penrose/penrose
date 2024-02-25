import { xml2js, xml2json } from "xml-js";
import { RawAlloyExported } from "../types/RawAlloyInstance.js";
import { safe, safeArray } from "../utils/Utils.js";
import {
  AlloyInstanceRel,
  AlloyInstanceSig,
  AlloyInstance,
} from "../types/AlloyInstance.js";

export const compileInstance = (instXml: string): AlloyInstance => {
  const raw = xml2js(instXml, { compact: true }) as RawAlloyExported;

  const instance = raw.alloy.instance;

  const idSigMap = new Map<number, AlloyInstanceSig>();

  for (const rawSig of safeArray(instance.sig)) {
    const id = Number(rawSig._attributes.ID);
    const name = rawSig._attributes.label;
    const atoms = safeArray(rawSig.atom).map((atom) => atom._attributes.label);

    idSigMap.set(id, { name, id, atoms });
  }

  const rels: AlloyInstanceRel[] = [];

  for (const rawField of safeArray(instance.field)) {
    const name = rawField._attributes.label;
    const belongsToId = Number(rawField._attributes.parentID);
    const belongsTo = safe(idSigMap.get(belongsToId)).name;
    const tuples = safeArray(rawField.tuple).map((tuple) =>
      safeArray(tuple.atom).map((atom) => atom._attributes.label),
    );
    rels.push({ name, belongsTo, tuples });
  }

  return {
    sigs: [...idSigMap.values()],
    rels,
  };
};
