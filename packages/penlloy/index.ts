import * as fs from "fs";
import {
  compileInstance,
  translateToSubstance,
} from "./generator/substance.js";

const instXml = fs.readFileSync("inst.xml", "utf8");
fs.writeFileSync(
  "inst.sub",
  translateToSubstance(compileInstance(instXml)),
  "utf8",
);
