module.exports = {
  testMatch: ["**/test/**/*.test.ts"],
  testEnvironment: "node",
  transform: {
    "^.+\\.(ts|tsx)$": [
      "ts-jest",
      {
        tsconfig: "./tsconfig.json",
      },
    ],
  },
  moduleNameMapper: {
    "^~/(.*)$": "<rootDir>/src/$1",
  },
  testTimeout: 30000,
  setupFilesAfterEnv: ["<rootDir>/test/jest.setup.ts"],
};
