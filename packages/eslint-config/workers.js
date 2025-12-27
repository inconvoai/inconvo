import { config } from "./base.js";

/**
 * A shared ESLint configuration for Cloudflare Workers.
 *
 * @type {import("eslint").Linter.Config[]}
 * */
export const workersConfig = [
  ...config,
  {
    ignores: [".wrangler/**", "worker-configuration.d.ts"],
  },
];
