/**
 * Build script that compiles the Next.js dev-server into a single Bun executable.
 *
 * What this does (replacing the next-bun-compile dependency):
 *  1. Build Next.js standalone output via `next build`
 *  2. Generate SQLite DDL for embedded DB migration
 *  3. Discover and embed static/public/runtime assets using Bun's `import with { type: "file" }`
 *  4. Generate a server entry point that extracts assets on first run
 *  5. Patch Next.js require hooks for binary compat
 *  6. Run `bun build --compile` for each target platform
 *
 * Usage:
 *   bun run scripts/build-binary.ts                     # build for current platform
 *   bun run scripts/build-binary.ts bun-darwin-arm64    # build for specific target
 *   bun run scripts/build-binary.ts all                 # build all 4 targets
 */

import {
  existsSync,
  readdirSync,
  statSync,
  readFileSync,
  writeFileSync,
  mkdirSync,
  copyFileSync,
} from "node:fs";
import { join, relative, dirname } from "node:path";
import { createHash } from "node:crypto";
import { execFileSync, execSync } from "node:child_process";

const PROJECT_DIR = join(import.meta.dirname, "..");
const STANDALONE_DIR = join(PROJECT_DIR, ".next", "standalone");
const DIST_DIR = join(PROJECT_DIR, "dist");

const ALL_TARGETS = [
  "bun-darwin-arm64",
  "bun-darwin-x64",
  "bun-linux-x64",
  "bun-linux-arm64",
];

// ---------------------------------------------------------------------------
// Utility helpers
// ---------------------------------------------------------------------------

interface FileEntry {
  absolutePath: string;
  relativePath: string;
  urlPath: string;
}

function walkDir(dir: string, base: string = dir): FileEntry[] {
  const results: FileEntry[] = [];
  if (!existsSync(dir)) return results;
  for (const entry of readdirSync(dir)) {
    const full = join(dir, entry);
    if (statSync(full).isDirectory()) {
      results.push(...walkDir(full, base));
    } else {
      results.push({
        absolutePath: full,
        relativePath: relative(base, full),
        urlPath: "",
      });
    }
  }
  return results;
}

function toVarName(filePath: string): string {
  const hash = createHash("md5").update(filePath).digest("hex").slice(0, 8);
  const safe = filePath.replace(/[^a-zA-Z0-9]/g, "_").slice(0, 40);
  return `asset_${safe}_${hash}`;
}

// ---------------------------------------------------------------------------
// Find the server directory inside standalone output
// ---------------------------------------------------------------------------

function findServerDir(standaloneDir: string): string {
  if (existsSync(join(standaloneDir, "server.js"))) return standaloneDir;

  function search(dir: string): string | null {
    if (!existsSync(dir)) return null;
    for (const entry of readdirSync(dir)) {
      if (entry === "node_modules") continue;
      const full = join(dir, entry);
      if (!statSync(full).isDirectory()) continue;
      if (existsSync(join(full, "server.js"))) return full;
      const found = search(full);
      if (found) return found;
    }
    return null;
  }

  const found = search(standaloneDir);
  if (!found)
    throw new Error("Could not find server.js in standalone output");
  return found;
}

// ---------------------------------------------------------------------------
// Create stub modules for dev-only / optional deps
// ---------------------------------------------------------------------------

function generateStubs(standaloneDir: string): void {
  const nodeModulesDir = join(standaloneDir, "node_modules");
  const stubs = [
    {
      path: join(nodeModulesDir, "next/dist/server/dev/next-dev-server.js"),
      content: "module.exports = { default: null };",
    },
    {
      path: join(
        nodeModulesDir,
        "next/dist/server/lib/router-utils/setup-dev-bundler.js"
      ),
      content: "module.exports = {};",
    },
    {
      path: join(nodeModulesDir, "@opentelemetry/api/index.js"),
      content: "throw new Error('not installed');",
    },
    {
      path: join(nodeModulesDir, "critters/index.js"),
      content: "module.exports = {};",
    },
  ];

  let count = 0;
  for (const stub of stubs) {
    if (!existsSync(stub.path)) {
      mkdirSync(dirname(stub.path), { recursive: true });
      writeFileSync(stub.path, stub.content);
      count++;
    }
  }
  if (count > 0) console.log(`Created ${count} module stubs`);
}

// ---------------------------------------------------------------------------
// Patch Next.js require-hook.js so require.resolve doesn't crash in binary
// ---------------------------------------------------------------------------

function patchRequireHook(standaloneDir: string): void {
  const hookPath = join(
    standaloneDir,
    "node_modules/next/dist/server/require-hook.js"
  );
  if (!existsSync(hookPath)) return;

  let content = readFileSync(hookPath, "utf-8");
  const target =
    "let resolve = process.env.NEXT_MINIMAL ? __non_webpack_require__.resolve : require.resolve;";
  if (!content.includes(target)) return;

  const replacement =
    "let _resolve = process.env.NEXT_MINIMAL ? __non_webpack_require__.resolve : require.resolve;\n" +
    "let resolve = (id) => { try { return _resolve(id); } catch { return ''; } };";
  content = content.replace(target, replacement);
  writeFileSync(hookPath, content);
  console.log("Patched require-hook.js for binary compatibility");
}

// ---------------------------------------------------------------------------
// Trace external modules required by server chunks
// ---------------------------------------------------------------------------

function collectExternalModules(
  standaloneDir: string,
  serverDir: string
): string[] {
  const chunksDir = join(serverDir, ".next/server/chunks");
  if (!existsSync(chunksDir)) return [];

  const seeds = new Set<string>();
  for (const { absolutePath } of walkDir(chunksDir)) {
    if (!absolutePath.endsWith(".js")) continue;
    const content = readFileSync(absolutePath, "utf-8");
    for (const match of content.matchAll(/require\("(next\/dist\/[^"]+)"\)/g)) {
      if (match[1]) seeds.add(match[1]);
    }
  }

  const deps = new Set<string>();
  function trace(file: string): void {
    if (deps.has(file)) return;
    let fullPath = join(standaloneDir, "node_modules", file);
    if (existsSync(fullPath) && statSync(fullPath).isDirectory()) {
      const pkgJson = join(fullPath, "package.json");
      if (existsSync(pkgJson)) deps.add(file + "/package.json");
      file = file + "/index.js";
      fullPath = join(standaloneDir, "node_modules", file);
    }
    if (!existsSync(fullPath)) return;
    deps.add(file);
    const content = readFileSync(fullPath, "utf-8");
    for (const match of content.matchAll(/require\("([^"]+)"\)/g)) {
      const req = match[1]!;
      let resolved: string | undefined;
      if (req.startsWith(".")) {
        resolved = join(file, "..", req).replace(/\\/g, "/");
        if (!resolved.endsWith(".js")) resolved += ".js";
      } else if (req.startsWith("next/")) {
        resolved = req;
        if (!resolved.endsWith(".js")) resolved += ".js";
      }
      if (resolved) trace(resolved);
    }
  }

  for (const seed of seeds) trace(seed);
  return [...deps];
}

// ---------------------------------------------------------------------------
// Generate the entry point and asset map
// ---------------------------------------------------------------------------

function generateEntryPoint(standaloneDir: string): string {
  const serverDir = findServerDir(standaloneDir);

  generateStubs(standaloneDir);
  patchRequireHook(standaloneDir);

  // Collect static assets
  const staticDir = join(serverDir, ".next/static");
  const staticFiles: FileEntry[] = walkDir(staticDir).map((f) => ({
    ...f,
    urlPath: `/_next/static/${f.relativePath.replace(/\\/g, "/")}`,
  }));

  // Collect public assets
  const publicDir = join(PROJECT_DIR, "public");
  const publicFiles: FileEntry[] = walkDir(publicDir).map((f) => ({
    ...f,
    urlPath: `/${f.relativePath.replace(/\\/g, "/")}`,
  }));

  // Collect runtime files from standalone .next
  const standaloneNextDir = join(serverDir, ".next");
  const runtimeFiles: FileEntry[] = walkDir(standaloneNextDir).map((f) => ({
    ...f,
    urlPath: `__runtime/.next/${f.relativePath.replace(/\\/g, "/")}`,
  }));

  // Trace and copy external modules
  const externalModules = collectExternalModules(standaloneDir, serverDir);
  const externalPaths = ["next/package.json", ...externalModules];
  const externalDir = join(serverDir, ".next/__external");
  for (const mod of externalPaths) {
    const src = join(standaloneDir, "node_modules", mod);
    if (!existsSync(src)) continue;
    const dest = join(externalDir, mod);
    mkdirSync(dirname(dest), { recursive: true });
    copyFileSync(src, dest);
    runtimeFiles.push({
      absolutePath: dest,
      relativePath: `__external/${mod}`,
      urlPath: `__runtime/.next/node_modules/${mod.replace(/\\/g, "/")}`,
    });
  }

  const allAssets = [...staticFiles, ...publicFiles, ...runtimeFiles];

  // Generate assets.generated.js with Bun file embeds
  const imports: string[] = [];
  const mapEntries: string[] = [];
  for (const asset of allAssets) {
    const varName = toVarName(asset.urlPath);
    const importPath = relative(serverDir, asset.absolutePath).replace(
      /\\/g,
      "/"
    );
    imports.push(
      `import ${varName} from "./${importPath}" with { type: "file" };`
    );
    mapEntries.push(`  ["${asset.urlPath}", ${varName}],`);
  }
  writeFileSync(
    join(serverDir, "assets.generated.js"),
    `${imports.join("\n")}\nexport const assetMap = new Map([\n${mapEntries.join("\n")}\n]);\n`
  );
  console.log(`Embedded ${allAssets.length} assets`);

  // Extract nextConfig from standalone server.js
  const serverSrc = readFileSync(join(serverDir, "server.js"), "utf-8");
  const configMatch = serverSrc.match(/const nextConfig = ({[\s\S]*?})\n/);
  if (!configMatch)
    throw new Error("Could not extract nextConfig from standalone server.js");

  // Build asset extraction map
  const extractions = allAssets.map((a) => {
    let diskPath: string;
    if (a.urlPath.startsWith("__runtime/")) {
      diskPath = a.urlPath.slice("__runtime/".length);
    } else if (a.urlPath.startsWith("/_next/static/")) {
      diskPath = ".next/static/" + a.relativePath;
    } else {
      diskPath = "public/" + a.relativePath;
    }
    return [a.urlPath, diskPath];
  });

  // Generate server-entry.js
  const serverEntry = `import { assetMap } from "./assets.generated.js";
const path = require("path");
const fs = require("fs");

const baseDir = path.dirname(process.execPath);
process.chdir(baseDir);
process.env.NODE_ENV = "production";

const nextConfig = ${configMatch[1]};
process.env.__NEXT_PRIVATE_STANDALONE_CONFIG = JSON.stringify(nextConfig);

const currentPort = parseInt(process.env.PORT, 10) || 3000;
const hostname = process.env.HOSTNAME || "0.0.0.0";
let keepAliveTimeout = parseInt(process.env.KEEP_ALIVE_TIMEOUT, 10);
if (Number.isNaN(keepAliveTimeout) || !Number.isFinite(keepAliveTimeout) || keepAliveTimeout < 0) {
  keepAliveTimeout = undefined;
}

const extractions = ${JSON.stringify(extractions)};
async function extractAssets() {
  let n = 0;
  for (const [urlPath, diskPath] of extractions) {
    const fullPath = path.join(baseDir, diskPath);
    if (fs.existsSync(fullPath)) continue;
    fs.mkdirSync(path.dirname(fullPath), { recursive: true });
    const embedded = assetMap.get(urlPath);
    if (embedded) { await Bun.write(fullPath, Bun.file(embedded)); n++; }
  }
  if (n > 0) console.log(\`Extracted \${n} assets\`);
}

extractAssets().then(() => {
  require("next");
  const { startServer } = require("next/dist/server/lib/start-server");
  return startServer({
    dir: baseDir, isDev: false, config: nextConfig,
    hostname, port: currentPort, allowRetry: false, keepAliveTimeout,
  });
}).catch((err) => { console.error(err); process.exit(1); });
`;

  writeFileSync(join(serverDir, "server-entry.js"), serverEntry);
  console.log("Generated server-entry.js");

  return serverDir;
}

// ---------------------------------------------------------------------------
// Compile to binary using bun build --compile
// ---------------------------------------------------------------------------

function compile(
  serverDir: string,
  outfile: string,
  extraArgs: string[] = []
): void {
  const entryPoint = join(serverDir, "server-entry.js");
  const args = [
    "build",
    entryPoint,
    "--production",
    "--compile",
    "--minify",
    "--bytecode",
    "--sourcemap",
    "--define",
    "process.env.TURBOPACK=1",
    "--define",
    "process.env.__NEXT_EXPERIMENTAL_REACT=",
    "--define",
    'process.env.NEXT_RUNTIME="nodejs"',
    "--outfile",
    outfile,
    ...extraArgs,
  ];

  console.log(`Compiling to ${outfile}...`);
  execFileSync("bun", args, { stdio: "inherit" });
  console.log(`Done → ${outfile}`);
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

const args = process.argv.slice(2);
let targets: string[];

if (args.length === 0) {
  // Default: build for current platform only
  const platform = process.platform === "darwin" ? "darwin" : "linux";
  const arch = process.arch === "arm64" ? "arm64" : "x64";
  targets = [`bun-${platform}-${arch}`];
} else if (args[0] === "all") {
  targets = ALL_TARGETS;
} else {
  targets = args;
}

// Step 1: Build Next.js standalone
console.log("\n=== Building Next.js standalone ===\n");
execSync("SKIP_ENV_VALIDATION=true npx next build", {
  cwd: PROJECT_DIR,
  stdio: "inherit",
});

if (!existsSync(STANDALONE_DIR)) {
  console.error("No standalone output found at", STANDALONE_DIR);
  process.exit(1);
}

// Step 2: Generate SQLite DDL for embedded migration
console.log("\n=== Generating SQLite DDL ===\n");
execSync(
  "npx prisma migrate diff --from-empty --to-schema-datamodel prisma/schema.prisma --script > src/generated-ddl.sql",
  { cwd: PROJECT_DIR, stdio: "inherit" }
);

// Step 3: Generate entry point and asset map
console.log("\n=== Generating entry point ===\n");
const serverDir = generateEntryPoint(STANDALONE_DIR);

// Step 4: Compile for each target
mkdirSync(DIST_DIR, { recursive: true });
for (const target of targets) {
  console.log(`\n=== Compiling for ${target} ===\n`);
  const outfile = join(DIST_DIR, `inconvo-dev-server-${target}`);
  compile(serverDir, outfile, [`--target=${target}`]);
}

console.log("\n=== Build complete ===\n");
for (const target of targets) {
  const outfile = join(DIST_DIR, `inconvo-dev-server-${target}`);
  if (existsSync(outfile)) {
    const size = statSync(outfile).size;
    console.log(
      `  ${target}: ${(size / 1024 / 1024).toFixed(1)}MB`
    );
  }
}
