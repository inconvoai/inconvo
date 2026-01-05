import fs from "fs/promises";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/**
 * Returns the test fixtures schema-augmentations directory.
 * This is where tests should read/write augmentation files.
 */
export function resolveAugmentationsDir(_dirname: string): string {
  return path.resolve(__dirname, "../fixtures/schema-augmentations");
}

export async function ensureAugmentationsDir(dir: string) {
  await fs.mkdir(dir, { recursive: true });
}

export async function writeAugmentationFile(
  filePath: string,
  payload: unknown,
) {
  await ensureAugmentationsDir(path.dirname(filePath));
  await fs.writeFile(filePath, JSON.stringify(payload, null, 2), "utf8");
}

export async function removeAugmentationFile(filePath: string) {
  try {
    await fs.unlink(filePath);
  } catch (error: any) {
    if (error?.code !== "ENOENT") {
      throw error;
    }
  }
}
