import { jest } from "@jest/globals";
import * as fs from "fs";
// @ts-ignore
import { defineInlineTest } from "jscodeshift/dist/testUtils";
import * as path from "path";
import { dirname } from "path";
import { fileURLToPath } from "url";
import * as transform from "../utils/toCustomAD";
jest.autoMockOff(); // idk if this is required
// const defineTest = require("jscodeshift/dist/testUtils").defineTest; // i know we don't use require but i could not get it to work with import
// may not be a typing for that specific file
// https://stackoverflow.com/a/62892482
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
defineInlineTest(
  transform,
  {
    parser: "ts",
  },
  fs.readFileSync(
    path.join(__dirname, "../__testfixtures__/transformtest.input.ts"),
    "utf8"
  ),
  fs.readFileSync(
    path.join(__dirname, "../__testfixtures__/transformtest.output.ts"),
    "utf8"
  )
); // docs for this are here: https://github.com/facebook/jscodeshift
