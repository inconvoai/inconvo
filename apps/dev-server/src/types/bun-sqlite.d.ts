/**
 * Minimal type declarations for bun:sqlite.
 * Only the subset used by this project is declared here.
 */
declare module "bun:sqlite" {
  export class Database {
    constructor(filename: string);
    exec(sql: string): void;
    query<T = unknown, P extends unknown[] = unknown[]>(
      sql: string
    ): Statement<T, P>;
    prepare<T = unknown, P extends unknown[] = unknown[]>(
      sql: string
    ): Statement<T, P>;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    transaction<T extends (...args: any[]) => any>(fn: T): T;
    close(): void;
  }

  export class Statement<T = unknown, P extends unknown[] = unknown[]> {
    get(...params: P): T | null;
    all(...params: P): T[];
    run(...params: P): void;
  }
}
