/** @type {import('ts-jest').JestConfigWithTsJest} **/
module.exports = {
  moduleNameMapper: {
    "^~/(.*)$": "<rootDir>/src/$1",
  },
  testEnvironment: "node",
  preset: "ts-jest",
  globalSetup: "<rootDir>/jest.global-setup.ts",
};
