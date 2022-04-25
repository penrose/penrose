export default {
  preset: "ts-jest",
  testEnvironment: "jest-bench/environment",
  reporters: ["default", "jest-bench/reporter"],
  transform: {
    "^.+\\.(ts|tsx)?$": "ts-jest",
  },
  testEnvironmentOptions: {
    testEnvironment: "jest-environment-jsdom",
  },
  testRegex: "\\.bench\\.[jt]sx?$",
  modulePaths: ["node_modules", "<rootDir>/src/"],
  testPathIgnorePatterns: ["build/dist/"],
  // coverageProvider: "v8",
  // coveragePathIgnorePatterns: ["/node_modules/", "/test/"],
  // collectCoverageFrom: ["src/**/*.ts"],
};
