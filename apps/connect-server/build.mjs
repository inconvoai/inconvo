import * as esbuild from "esbuild";

// Packages that must be external due to:
// - Native bindings (mysql2, pg-native)
// - Dynamic require at runtime (pino transports)
// - grpc/protobuf issues (@google-cloud/*)
const MUST_BE_EXTERNAL = [
  "pino",
  "pino-pretty",
  "pg",
  "pg-native",
  "mysql2",
  "tedious",
  "@google-cloud/*",
];

await esbuild.build({
  entryPoints: ["src/index.ts"],
  bundle: true,
  platform: "node",
  target: "node22",
  format: "cjs",
  outfile: "dist/index.cjs",
  external: MUST_BE_EXTERNAL,
  // Provide import.meta shim for CJS
  banner: {
    js: `const importMetaUrl = require('url').pathToFileURL(__filename).href;`,
  },
  define: {
    "import.meta.url": "importMetaUrl",
  },
});

console.log("Build complete");
