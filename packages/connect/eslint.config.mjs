import { config } from "@repo/eslint-config/base";

/** @type {import("eslint").Linter.Config[]} */
export default [
  ...config,
  {
    rules: {
      // Many anys exist - warn for now, fix incrementally
      "@typescript-eslint/no-explicit-any": "warn",
      "@typescript-eslint/no-unused-vars": [
        "warn",
        {
          argsIgnorePattern: "^_",
          varsIgnorePattern: "^_",
        },
      ],
      // Runtime env vars, not build-time
      "turbo/no-undeclared-env-vars": "off",
    },
  },
  {
    ignores: ["dist/**", "test/**"],
  },
];
