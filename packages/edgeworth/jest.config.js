/** @type {import('ts-jest/dist/types').InitialOptionsTsJest} */
module.exports = {
  preset: "ts-jest",
  testEnvironment: "jsdom",
  globals: { "ts-jest": { useESM: true } },
  testEnvironment: "node",
  modulePaths: ["node_modules", "<rootDir>/src/"],
  testPathIgnorePatterns: ["dist/"],
  extensionsToTreatAsEsm: [".ts"],
};
