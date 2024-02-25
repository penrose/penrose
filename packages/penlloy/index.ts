import * as fs from "fs";
import { RawAlloyModel } from "./types/RawAlloyModel.js";
import { compileModel, translateToDomain } from "./generator/domain.js";
import { compileInstance } from "./generator/substance.js";

const instXml = fs.readFileSync("inst.xml", "utf8");

fs.writeFileSync("inst.json", compileInstance(instXml), "utf8");
