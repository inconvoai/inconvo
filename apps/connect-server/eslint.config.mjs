import { config } from "@repo/eslint-config/base";

/** @type {import("eslint").Linter.Config[]} */
export default [
  ...config,
  {
    rules: {
      // Runtime env vars, not build-time
      "turbo/no-undeclared-env-vars": "off",
    },
  },
];
