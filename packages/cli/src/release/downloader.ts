import * as fs from "fs/promises";
import * as path from "path";
import * as os from "os";
import { createWriteStream } from "fs";
import { execSync } from "child_process";
import * as p from "@clack/prompts";

// TODO: Update this when the repo is made public
const GITHUB_REPO = process.env.INCONVO_GITHUB_REPO || "ten-dev/inconvo";

/**
 * Check if gh CLI is available and authenticated
 */
function isGhCliAvailable(): boolean {
  try {
    execSync("gh auth status", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

/**
 * Download release using gh CLI (works with private repos if user is authenticated)
 */
function downloadWithGhCli(
  version: string,
  tarballPath: string
): boolean {
  try {
    const patterns = [
      `inconvo-v${version}-bundle.tar.gz`,
      `inconvo-${version}-bundle.tar.gz`,
    ];

    for (const pattern of patterns) {
      try {
        execSync(
          `gh release download "v${version}" --repo "${GITHUB_REPO}" --pattern "${pattern}" --output "${tarballPath}"`,
          { stdio: "pipe" }
        );
        return true;
      } catch {
        // Try next pattern
      }
    }
    return false;
  } catch {
    return false;
  }
}

/**
 * Get latest version using gh CLI (works with private repos)
 */
function getLatestVersionWithGhCli(): string | null {
  try {
    const result = execSync(
      `gh release view --repo "${GITHUB_REPO}" --json tagName -q ".tagName"`,
      { stdio: "pipe", encoding: "utf-8" }
    );
    return result.trim().replace(/^v/, "");
  } catch {
    return null;
  }
}
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
  // Try gh CLI first (works with private repos)
  if (isGhCliAvailable()) {
    const version = getLatestVersionWithGhCli();
    if (version) return version;
  }

  // Fall back to public API
  const response = await fetch(
    `https://api.github.com/repos/${GITHUB_REPO}/releases/latest`,
    {
      headers: {
        Accept: "application/vnd.github.v3+json",
        "User-Agent": "inconvo-cli",
      },
    }
  );

  if (!response.ok) {
    throw new Error(
      `Failed to fetch latest release from GitHub (${response.status}).\n` +
        `The repository may be private or no releases exist yet.\n` +
        `Repo: ${GITHUB_REPO}`
    );
  }

  const data = (await response.json()) as { tag_name: string };
  return data.tag_name.replace(/^v/, "");
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

    const tarballPath = path.join(RELEASES_DIR, `${version}.tar.gz`);
    let downloaded = false;

    // Try gh CLI first (works with private repos if user is authenticated)
    if (isGhCliAvailable()) {
      downloaded = downloadWithGhCli(version, tarballPath);
    }

    // Fall back to direct fetch (works with public repos)
    if (!downloaded) {
      const bundleUrl = `https://github.com/${GITHUB_REPO}/releases/download/v${version}/inconvo-v${version}-bundle.tar.gz`;
      const bundleResponse = await fetch(bundleUrl, { redirect: "follow" });

      if (!bundleResponse.ok) {
        // Try alternate URL pattern without 'v' prefix
        const altUrl = `https://github.com/${GITHUB_REPO}/releases/download/v${version}/inconvo-${version}-bundle.tar.gz`;
        const altResponse = await fetch(altUrl, { redirect: "follow" });

        if (!altResponse.ok) {
          throw new Error(
            `Could not download release bundle for v${version}.\n` +
              `The release may not exist or the repository may be private.\n` +
              `Repo: ${GITHUB_REPO}\n` +
              `To use a different repo, set INCONVO_GITHUB_REPO environment variable.`
          );
        }

        await downloadToFile(altResponse, tarballPath);
      } else {
        await downloadToFile(bundleResponse, tarballPath);
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

    // Verify this is a pre-built standalone bundle (has server.js)
    const isStandalone = await checkFileExists(
      path.join(versionDir, "apps", "dev-server", "server.js")
    );

    if (!isStandalone) {
      throw new Error(
        `Downloaded bundle for v${version} is not a valid pre-built release.\n` +
          `Expected to find apps/dev-server/server.js in the bundle.`
      );
    }

    const devServerDir = path.join(versionDir, "apps", "dev-server");
    const sandboxDir = path.join(versionDir, "apps", "sandbox");

    // Mark as installed
    await fs.writeFile(
      path.join(versionDir, ".installed"),
      JSON.stringify({ version, installedAt: new Date().toISOString() })
    );

    spinner.stop(`Inconvo v${version} installed successfully`);

    // Clean up old versions to avoid disk bloat
    await cleanOldVersions(2);

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
 * Compare two semver version strings
 */
function compareSemver(a: string, b: string): number {
  const parseVersion = (v: string) => {
    const dashIndex = v.indexOf("-");
    const main = dashIndex === -1 ? v : v.slice(0, dashIndex);
    const prerelease = dashIndex === -1 ? undefined : v.slice(dashIndex + 1);
    const parts = main.split(".").map((n) => parseInt(n, 10) || 0);
    return { parts, prerelease };
  };

  const va = parseVersion(a);
  const vb = parseVersion(b);

  // Compare major.minor.patch
  for (let i = 0; i < Math.max(va.parts.length, vb.parts.length); i++) {
    const diff = (va.parts[i] || 0) - (vb.parts[i] || 0);
    if (diff !== 0) return diff;
  }

  // If one has prerelease and other doesn't, non-prerelease is newer
  if (va.prerelease && !vb.prerelease) return -1;
  if (!va.prerelease && vb.prerelease) return 1;

  // Both have prerelease, compare lexically
  if (va.prerelease && vb.prerelease) {
    return va.prerelease.localeCompare(vb.prerelease);
  }

  return 0;
}

/**
 * Clean up old versions, keeping the N most recent
 */
export async function cleanOldVersions(keep: number = 2): Promise<void> {
  const versions = await listInstalledVersions();
  if (versions.length <= keep) return;

  // Sort by semver (newest first)
  versions.sort((a, b) => compareSemver(b, a));

  // Remove old versions
  for (const version of versions.slice(keep)) {
    await removeVersion(version);
  }
}
