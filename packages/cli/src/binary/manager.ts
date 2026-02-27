import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { execSync } from "child_process";

const INCONVO_DIR = path.join(os.homedir(), ".inconvo");
const BIN_DIR = path.join(INCONVO_DIR, "bin");

// GitHub repository for release downloads
const GITHUB_REPO = "inconvoai/inconvo";

/**
 * Resolve the Bun target string for the current platform and architecture.
 */
function getTargetForPlatform(): string {
  const platform = process.platform === "darwin" ? "darwin" : "linux";
  const arch = process.arch === "arm64" ? "arm64" : "x64";
  return `bun-${platform}-${arch}`;
}

/**
 * Get the binary name for the current platform.
 */
function getBinaryName(): string {
  return `inconvo-dev-server-${getTargetForPlatform()}`;
}

/**
 * Get the path where the binary should be stored for a given version.
 */
function getBinaryPath(version: string): string {
  return path.join(BIN_DIR, version, getBinaryName());
}

/**
 * Check if the binary already exists for a given version.
 */
function binaryExists(version: string): boolean {
  return fs.existsSync(getBinaryPath(version));
}

/**
 * Download the binary from GitHub Releases.
 */
async function downloadBinary(
  version: string,
  onProgress?: (message: string) => void,
): Promise<string> {
  const binaryName = getBinaryName();
  const binaryPath = getBinaryPath(version);
  const versionDir = path.dirname(binaryPath);

  // Create version directory
  fs.mkdirSync(versionDir, { recursive: true });

  const downloadUrl = `https://github.com/${GITHUB_REPO}/releases/download/v${version}/${binaryName}`;
  onProgress?.(`Downloading ${binaryName}...`);

  const response = await fetch(downloadUrl);
  if (!response.ok) {
    throw new Error(
      `Failed to download binary: ${response.status} ${response.statusText}\n` +
        `URL: ${downloadUrl}\n` +
        `Make sure version v${version} has been released with compiled binaries.`,
    );
  }

  const buffer = await response.arrayBuffer();
  fs.writeFileSync(binaryPath, Buffer.from(buffer));
  fs.chmodSync(binaryPath, 0o755);

  onProgress?.(`Downloaded ${binaryName} (${(buffer.byteLength / 1024 / 1024).toFixed(1)}MB)`);
  return binaryPath;
}

/**
 * Ensure the correct binary version is available, downloading if needed.
 * Returns the absolute path to the binary.
 */
export async function ensureBinary(
  version: string,
  onProgress?: (message: string) => void,
): Promise<string> {
  if (binaryExists(version)) {
    return getBinaryPath(version);
  }
  return downloadBinary(version, onProgress);
}

/**
 * Get the version of the CLI package (used as default binary version).
 */
export function getCliVersion(): string {
  // Try to read from package.json at CLI root
  try {
    const pkgPath = path.join(
      path.dirname(new URL(import.meta.url).pathname),
      "..",
      "..",
      "package.json",
    );
    const pkg = JSON.parse(fs.readFileSync(pkgPath, "utf-8"));
    return pkg.version;
  } catch {
    return "latest";
  }
}

/**
 * Check if the current platform is supported.
 */
export function checkPlatformSupport(): void {
  const platform = process.platform;
  const arch = process.arch;

  const supported =
    (platform === "darwin" && (arch === "arm64" || arch === "x64")) ||
    (platform === "linux" && (arch === "arm64" || arch === "x64"));

  if (!supported) {
    throw new Error(
      `Unsupported platform: ${platform}-${arch}. ` +
        `Supported platforms: macOS (arm64, x64), Linux (arm64, x64).`,
    );
  }
}
