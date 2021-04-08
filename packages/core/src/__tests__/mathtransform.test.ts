// @ts-ignore
import { defineTest } from "jscodeshift/dist/testUtils";
jest.autoMockOff(); // idk if this is required
// const defineTest = require("jscodeshift/dist/testUtils").defineTest; // i know we don't use require but i could not get it to work with import
// may not be a typing for that specific file
defineTest(__dirname, "utils/toCustomAD", null, "transformtest", {
  parser: "ts",
}); // docs for this are here: https://github.com/facebook/jscodeshift
