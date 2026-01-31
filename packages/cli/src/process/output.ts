const COLORS = {
  cyan: "\x1b[36m",
  red: "\x1b[31m",
  gray: "\x1b[90m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  blue: "\x1b[34m",
  magenta: "\x1b[35m",
  bold: "\x1b[1m",
  dim: "\x1b[2m",
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

export function logDim(message: string): void {
  console.log(`${COLORS.dim}${message}${COLORS.reset}`);
}

export function logBold(message: string): void {
  console.log(`${COLORS.bold}${message}${COLORS.reset}`);
}

export function logCyan(message: string): void {
  console.log(`${COLORS.cyan}${message}${COLORS.reset}`);
}

export function logGreen(message: string): void {
  console.log(`${COLORS.green}${message}${COLORS.reset}`);
}

export { COLORS };
