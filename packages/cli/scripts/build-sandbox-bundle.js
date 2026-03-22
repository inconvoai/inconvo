import { build } from "esbuild";
import * as fs from "fs/promises";
import * as path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const cliRoot = path.resolve(__dirname, "..");
const monorepoRoot = path.resolve(cliRoot, "../..");
const sandboxRoot = path.join(monorepoRoot, "apps", "sandbox");
const outDir = path.join(cliRoot, "assets", "sandbox");
const outScriptsDir = path.join(outDir, "scripts");

const sandboxEntry = path.join(sandboxRoot, "src", "index.ts");
const wranglerSource = path.join(sandboxRoot, "wrangler.jsonc");
const dockerfileSource = path.join(sandboxRoot, "Dockerfile");
const helperSource = path.join(sandboxRoot, "scripts", "inconvo.py");

const mainRegex = /"main"\s*:\s*"[^"]+"/;
const schemaRegex = /"\$schema"\s*:\s*"[^"]+"/;

async function buildWorkerBundle() {
  await fs.rm(outDir, { recursive: true, force: true });
  await fs.mkdir(outScriptsDir, { recursive: true });

  await build({
    entryPoints: [sandboxEntry],
    outfile: path.join(outDir, "worker.js"),
    bundle: true,
    format: "esm",
    platform: "neutral",
    target: "es2022",
    external: ["cloudflare:workers"],
  });

  const wranglerConfig = await fs.readFile(wranglerSource, "utf-8");
  const patchedConfig = wranglerConfig
    .replace(schemaRegex, '"$schema": "https://developers.cloudflare.com/wrangler/config-schema.json"')
    .replace(mainRegex, '"main": "./worker.js"');

  await fs.writeFile(path.join(outDir, "wrangler.jsonc"), patchedConfig);
  await fs.copyFile(dockerfileSource, path.join(outDir, "Dockerfile"));
  await fs.copyFile(helperSource, path.join(outScriptsDir, "inconvo.py"));
}

buildWorkerBundle().catch((error) => {
  console.error("Failed to build sandbox bundle:", error);
  process.exit(1);
});
