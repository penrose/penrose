import * as fs from "fs";
import { RawAlloyModel } from "./types/RawAlloyModel.js";
import { compileModel, translateToDomain } from "./generator/domain.js";

const sigsJson = fs.readFileSync("sigs.json", "utf8");

const modelShape = JSON.parse(sigsJson) as RawAlloyModel;

fs.writeFileSync(
  "out.domain",
  translateToDomain(compileModel(modelShape)),
  "utf8",
);
