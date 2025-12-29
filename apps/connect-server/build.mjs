import * as esbuild from "esbuild";

// Plugin to externalize all packages except workspace packages
const externalizeNonWorkspace = {
  name: "externalize-non-workspace",
  setup(build) {
    // Externalize all bare imports that aren't workspace packages
    build.onResolve({ filter: /^[^./]/ }, (args) => {
      // Keep workspace packages to be bundled
      if (args.path.startsWith("@repo/")) {
        return null; // Let esbuild resolve normally
      }
      // Externalize everything else
      return { path: args.path, external: true };
    });
  },
};

await esbuild.build({
  entryPoints: ["src/index.ts"],
  bundle: true,
  platform: "node",
  target: "node22",
  format: "esm",
  outfile: "dist/index.js",
  plugins: [externalizeNonWorkspace],
});

console.log("Build complete");
