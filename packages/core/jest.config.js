module.exports = {
  preset: "ts-jest",
  testEnvironment: "jsdom",
  modulePaths: ["node_modules", "<rootDir>/src/"],
  testPathIgnorePatterns: ["build/dist/"],
  setupFiles: ["./setupJestEnv.js"], // https://stackoverflow.com/a/57943686
};
