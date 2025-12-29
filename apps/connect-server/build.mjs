import * as esbuild from "esbuild";
import { readFileSync } from "fs";

const pkg = JSON.parse(readFileSync("./package.json", "utf-8"));

// Get all non-workspace dependencies to externalize
const external = Object.keys(pkg.dependencies || {}).filter(
  (dep) => !dep.startsWith("@repo/")
);

await esbuild.build({
  entryPoints: ["src/index.ts"],
  bundle: true,
  platform: "node",
  target: "node22",
  format: "esm",
  outfile: "dist/index.js",
  external,
});

console.log("Build complete");
