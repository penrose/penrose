module.exports = {
  preset: "ts-jest",
  testEnvironment: "jsdom",
  modulePaths: ["node_modules", "<rootDir>/src/"],
  testPathIgnorePatterns: ["dist/"],
  extensionsToTreatAsEsm: [".ts"],
  globals: { "ts-jest": { useESM: true } },
  setupFiles: ["./setupJestEnv.js"], // https://stackoverflow.com/a/57943686
};
