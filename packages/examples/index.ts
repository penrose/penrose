import { Trio } from "@penrose/core";

import testTrios from "./src/testing-registry.json";

import loadProgram from "./loadLocalProgram";

export const loadTrioPrograms = async (
  trio: Trio,
  prefix = testTrios.root
): Promise<{ substance: string; style: string; domain: string }> => {
  // if (trio.path !)
  //   const uri = prefix + trio.path;
  // const substance = await fetch(prefix trio.substance);
  // const style = await loadStyle(trio.style);
  // const domain = await loadDomain(trio.domain);
  // return {substance, style, domain};
};

export { testTrios, loadProgram };
