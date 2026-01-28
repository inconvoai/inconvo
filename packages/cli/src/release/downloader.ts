import * as fs from "fs/promises";
import * as path from "path";
import * as os from "os";
import { createWriteStream } from "fs";
import { execSync } from "child_process";
import * as p from "@clack/prompts";

// TODO: Update this when the repo is made public
const GITHUB_REPO = process.env.INCONVO_GITHUB_REPO || "ten-dev/inconvo";
const INCONVO_DIR = path.join(os.homedir(), ".inconvo");
const RELEASES_DIR = path.join(INCONVO_DIR, "releases");

export interface ReleaseInfo {
  version: string;
  releaseDir: string;
  devServerDir: string;
  sandboxDir: string;
}

/**
 * Get the directory where a specific version is installed
 */
export function getVersionDir(version: string): string {
  return path.join(RELEASES_DIR, version);
}

/**
 * Check if a version is already downloaded
 */
export async function isVersionInstalled(version: string): Promise<boolean> {
  const versionDir = getVersionDir(version);
  try {
    await fs.access(path.join(versionDir, ".installed"));
    return true;
  } catch {
    return false;
  }
}

/**
 * Get the latest release version from GitHub
 */
export async function getLatestVersion(): Promise<string> {
  try {
    const response = await fetch(
      `https://api.github.com/repos/${GITHUB_REPO}/releases/latest`,
      {
        headers: {
          Accept: "application/vnd.github.v3+json",
          "User-Agent": "inconvo-cli",
        },
      }
    );

    if (response.ok) {
      const data = (await response.json()) as { tag_name: string };
      return data.tag_name.replace(/^v/, "");
    }
  } catch {
    // Fall through to fallback
  }

  // Fallback: use CLI version if no releases exist yet
  return getCliVersion();
}

/**
 * Get the CLI package version
 */
export function getCliVersion(): string {
  return "0.1.0";
}

/**
 * Download and extract a pre-built release bundle from GitHub
 */
export async function downloadRelease(version: string): Promise<ReleaseInfo> {
  const versionDir = getVersionDir(version);

  // Check if already installed
  if (await isVersionInstalled(version)) {
    return {
      version,
      releaseDir: versionDir,
      devServerDir: path.join(versionDir, "apps", "dev-server"),
      sandboxDir: path.join(versionDir, "apps", "sandbox"),
    };
  }

  const spinner = p.spinner();
  spinner.start(`Downloading Inconvo v${version}...`);

  try {
    // Ensure releases directory exists
    await fs.mkdir(RELEASES_DIR, { recursive: true });

    // Clean up any partial download
    try {
      await fs.rm(versionDir, { recursive: true });
    } catch {
      // Directory didn't exist
    }

    // Try to download the pre-built bundle from GitHub Releases
    const bundleUrl = `https://github.com/${GITHUB_REPO}/releases/download/v${version}/inconvo-v${version}-bundle.tar.gz`;
    const tarballPath = path.join(RELEASES_DIR, `${version}.tar.gz`);

    let downloaded = false;
    const bundleResponse = await fetch(bundleUrl, { redirect: "follow" });

    if (bundleResponse.ok) {
      await downloadToFile(bundleResponse, tarballPath);
      downloaded = true;
    } else {
      // Try alternate URL patterns
      const altUrls = [
        `https://github.com/${GITHUB_REPO}/releases/download/${version}/inconvo-${version}-bundle.tar.gz`,
        `https://github.com/${GITHUB_REPO}/releases/download/v${version}/inconvo-${version}-bundle.tar.gz`,
      ];

      for (const url of altUrls) {
        const response = await fetch(url, { redirect: "follow" });
        if (response.ok) {
          await downloadToFile(response, tarballPath);
          downloaded = true;
          break;
        }
      }
    }

    // Fall back to downloading source if no bundle exists
    if (!downloaded) {
      spinner.message("No pre-built bundle found, downloading source...");

      const sourceUrls = [
        `https://github.com/${GITHUB_REPO}/archive/refs/tags/v${version}.tar.gz`,
        `https://github.com/${GITHUB_REPO}/archive/refs/tags/${version}.tar.gz`,
        `https://github.com/${GITHUB_REPO}/archive/refs/heads/main.tar.gz`,
      ];

      for (const url of sourceUrls) {
        const response = await fetch(url, { redirect: "follow" });
        if (response.ok) {
          await downloadToFile(response, tarballPath);
          downloaded = true;
          break;
        }
      }

      if (!downloaded) {
        throw new Error(
          `Could not download release from GitHub.\n` +
            `The repository may be private or the release doesn't exist.\n` +
            `Tried: ${GITHUB_REPO}\n` +
            `To use a different repo, set INCONVO_GITHUB_REPO environment variable.`
        );
      }
    }

    spinner.message("Extracting...");

    // Extract the tarball
    await fs.mkdir(versionDir, { recursive: true });
    execSync(
      `tar -xzf "${tarballPath}" --strip-components=1 -C "${versionDir}"`,
      { stdio: "pipe" }
    );

    // Clean up tarball
    await fs.rm(tarballPath);

    // Check if this is a pre-built standalone bundle (has server.js) or source
    const isStandalone = await checkFileExists(
      path.join(versionDir, "apps", "dev-server", "server.js")
    );

    if (!isStandalone) {
      // Source download - need to install and build
      spinner.message("Installing dependencies (this may take a few minutes)...");

      // Check if pnpm is available
      try {
        execSync("pnpm --version", { stdio: "pipe" });
      } catch {
        throw new Error(
          "pnpm is required but not installed.\n" +
            "Install it with: npm install -g pnpm"
        );
      }

      // Install all dependencies from the monorepo root using pnpm
      execSync("pnpm install", {
        cwd: versionDir,
        stdio: "pipe",
      });

      // Build the dev-server
      spinner.message("Building dev-server...");
      execSync("npx next build", {
        cwd: path.join(versionDir, "apps", "dev-server"),
        stdio: "pipe",
        env: {
          ...process.env,
          SKIP_ENV_VALIDATION: "true",
        },
      });
    }
    // Pre-built standalone bundle - no install needed, deps are bundled

    const devServerDir = path.join(versionDir, "apps", "dev-server");
    const sandboxDir = path.join(versionDir, "apps", "sandbox");

    // Mark as installed
    await fs.writeFile(
      path.join(versionDir, ".installed"),
      JSON.stringify({ version, installedAt: new Date().toISOString() })
    );

    spinner.stop(`Inconvo v${version} installed successfully`);

    return {
      version,
      releaseDir: versionDir,
      devServerDir,
      sandboxDir,
    };
  } catch (error) {
    spinner.stop("Download failed");
    throw error;
  }
}

async function downloadToFile(
  response: Response,
  filePath: string
): Promise<void> {
  if (!response.body) {
    throw new Error("No response body");
  }

  const fileStream = createWriteStream(filePath);
  const reader = response.body.getReader();

  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      fileStream.write(value);
    }
  } finally {
    fileStream.end();
    await new Promise((resolve) => fileStream.on("finish", resolve));
  }
}

async function checkFileExists(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

/**
 * Get release info, downloading if necessary
 */
export async function ensureRelease(version?: string): Promise<ReleaseInfo> {
  const targetVersion = version || (await getLatestVersion());
  return downloadRelease(targetVersion);
}

/**
 * List installed versions
 */
export async function listInstalledVersions(): Promise<string[]> {
  try {
    const entries = await fs.readdir(RELEASES_DIR, { withFileTypes: true });
    const versions: string[] = [];

    for (const entry of entries) {
      if (entry.isDirectory()) {
        const installedMarker = path.join(
          RELEASES_DIR,
          entry.name,
          ".installed"
        );
        try {
          await fs.access(installedMarker);
          versions.push(entry.name);
        } catch {
          // Not fully installed
        }
      }
    }

    return versions;
  } catch {
    return [];
  }
}

/**
 * Remove a specific version
 */
export async function removeVersion(version: string): Promise<void> {
  const versionDir = getVersionDir(version);
  await fs.rm(versionDir, { recursive: true });
}

/**
 * Clean up old versions, keeping the N most recent
 */
export async function cleanOldVersions(keep: number = 2): Promise<void> {
  const versions = await listInstalledVersions();
  if (versions.length <= keep) return;

  // Sort by semver (simple string sort works for most cases)
  versions.sort().reverse();

  // Remove old versions
  for (const version of versions.slice(keep)) {
    await removeVersion(version);
  }
}
