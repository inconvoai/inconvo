const COLORS = {
  cyan: "\x1b[36m",
  yellow: "\x1b[33m",
  red: "\x1b[31m",
  gray: "\x1b[90m",
  reset: "\x1b[0m",
} as const;

export function createOutputHandler(
  name: string,
  color: keyof typeof COLORS
): (data: Buffer) => void {
  const colorCode = COLORS[color];
  const prefix = `${colorCode}[${name}]${COLORS.reset}`;

  return (data: Buffer) => {
    const lines = data.toString().split("\n");
    for (const line of lines) {
      if (line.trim()) {
        console.log(`${prefix} ${line}`);
      }
    }
  };
}

export function logInfo(message: string): void {
  console.log(`${COLORS.cyan}[inconvo]${COLORS.reset} ${message}`);
}

export function logError(message: string): void {
  console.error(`${COLORS.red}[inconvo]${COLORS.reset} ${message}`);
}

export function logGray(message: string): void {
  console.log(`${COLORS.gray}${message}${COLORS.reset}`);
}
