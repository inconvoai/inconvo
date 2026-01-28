#!/usr/bin/env node
/* global console, process */

/**
 * Resolve catalog: references in package.json before npm publish.
 *
 * This script reads the pnpm-workspace.yaml catalog and replaces
 * any "catalog:" version references with their actual versions.
 *
 * Run automatically via prepublishOnly hook.
 */

import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";
import YAML from "yaml";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const CLI_ROOT = path.resolve(__dirname, "..");
const MONOREPO_ROOT = path.resolve(CLI_ROOT, "../..");

function main() {
  console.log("Resolving catalog: references for npm publish...");

  // Read pnpm-workspace.yaml
  const workspacePath = path.join(MONOREPO_ROOT, "pnpm-workspace.yaml");
  if (!fs.existsSync(workspacePath)) {
    console.error("Error: pnpm-workspace.yaml not found at", workspacePath);
    process.exit(1);
  }

  const workspaceContent = fs.readFileSync(workspacePath, "utf-8");
  const workspace = YAML.parse(workspaceContent);
  const catalog = workspace.catalog || {};

  console.log(`Found ${Object.keys(catalog).length} packages in catalog`);

  // Read package.json
  const pkgPath = path.join(CLI_ROOT, "package.json");
  const pkg = JSON.parse(fs.readFileSync(pkgPath, "utf-8"));

  let replacements = 0;

  // Resolve catalog: references in dependencies
  for (const depType of ["dependencies", "devDependencies", "peerDependencies", "optionalDependencies"]) {
    if (!pkg[depType]) continue;

    for (const [name, version] of Object.entries(pkg[depType])) {
      if (version === "catalog:") {
        if (catalog[name]) {
          pkg[depType][name] = `^${catalog[name]}`;
          console.log(`  ${name}: catalog: → ^${catalog[name]}`);
          replacements++;
        } else {
          console.error(`  Warning: ${name} not found in catalog`);
        }
      }
    }
  }

  // Remove workspace: references (devDependencies won't be installed anyway)
  for (const depType of ["devDependencies"]) {
    if (!pkg[depType]) continue;

    for (const [name, version] of Object.entries(pkg[depType])) {
      if (typeof version === "string" && version.startsWith("workspace:")) {
        delete pkg[depType][name];
        console.log(`  Removed ${name} (workspace dependency)`);
      }
    }
  }

  if (replacements === 0) {
    console.log("No catalog: references to resolve");
    return;
  }

  // Write updated package.json
  fs.writeFileSync(pkgPath, JSON.stringify(pkg, null, 2) + "\n");
  console.log(`\nResolved ${replacements} catalog: references`);
}

main();
