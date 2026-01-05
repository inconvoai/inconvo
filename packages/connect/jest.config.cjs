module.exports = {
  testMatch: ["**/test/**/*.test.ts"],
  testEnvironment: "node",
  extensionsToTreatAsEsm: [".ts"],
  injectGlobals: true,
  transform: {
    "^.+\\.(ts|tsx)$": [
      "ts-jest",
      {
        tsconfig: "./tsconfig.json",
        useESM: true,
      },
    ],
  },
  moduleNameMapper: {
    "^~/(.*)$": "<rootDir>/src/$1",
  },
  testTimeout: 30000,
  setupFilesAfterEnv: ["<rootDir>/test/jest.setup.ts"],
};
