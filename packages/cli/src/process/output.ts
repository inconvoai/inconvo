const COLORS = {
  cyan: "\x1b[36m",
  red: "\x1b[31m",
  gray: "\x1b[90m",
  reset: "\x1b[0m",
} as const;

export function logInfo(message: string): void {
  console.log(`${COLORS.cyan}[inconvo]${COLORS.reset} ${message}`);
}

export function logError(message: string): void {
  console.error(`${COLORS.red}[inconvo]${COLORS.reset} ${message}`);
}

export function logGray(message: string): void {
  console.log(`${COLORS.gray}${message}${COLORS.reset}`);
}
