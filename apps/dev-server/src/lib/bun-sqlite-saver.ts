import { Database } from "bun:sqlite";
import {
  BaseCheckpointSaver,
  type Checkpoint,
  type CheckpointListOptions,
  type CheckpointTuple,
  type CheckpointMetadata,
  type CheckpointPendingWrite,
  type PendingWrite,
} from "@langchain/langgraph-checkpoint";
// RunnableConfig is from @langchain/core/runnables but may not be directly resolvable.
// Re-use the type via the checkpoint package's re-export in CheckpointTuple.
type RunnableConfig = NonNullable<CheckpointTuple["config"]>;

// SQL statements matching the schema used by @langchain/langgraph-checkpoint-sqlite
const SETUP_SQL = `
PRAGMA journal_mode=WAL;
CREATE TABLE IF NOT EXISTS checkpoints (
  thread_id TEXT NOT NULL,
  checkpoint_ns TEXT NOT NULL DEFAULT '',
  checkpoint_id TEXT NOT NULL,
  parent_checkpoint_id TEXT,
  type TEXT,
  checkpoint BLOB,
  metadata BLOB,
  PRIMARY KEY (thread_id, checkpoint_ns, checkpoint_id)
);
CREATE TABLE IF NOT EXISTS writes (
  thread_id TEXT NOT NULL,
  checkpoint_ns TEXT NOT NULL DEFAULT '',
  checkpoint_id TEXT NOT NULL,
  task_id TEXT NOT NULL,
  idx INTEGER NOT NULL,
  channel TEXT NOT NULL,
  type TEXT,
  value BLOB,
  PRIMARY KEY (thread_id, checkpoint_ns, checkpoint_id, task_id, idx)
);
`;

const SELECT_SQL = `
SELECT
  thread_id, checkpoint_ns, checkpoint_id, parent_checkpoint_id,
  type, checkpoint, metadata,
  (
    SELECT json_group_array(
      json_object(
        'task_id', pw.task_id,
        'channel', pw.channel,
        'type', pw.type,
        'value', CAST(pw.value AS TEXT)
      )
    )
    FROM writes AS pw
    WHERE pw.thread_id = checkpoints.thread_id
      AND pw.checkpoint_ns = checkpoints.checkpoint_ns
      AND pw.checkpoint_id = checkpoints.checkpoint_id
  ) AS pending_writes,
  (
    SELECT json_group_array(
      json_object(
        'type', ps.type,
        'value', CAST(ps.value AS TEXT)
      )
    )
    FROM writes AS ps
    WHERE ps.thread_id = checkpoints.thread_id
      AND ps.checkpoint_ns = checkpoints.checkpoint_ns
      AND ps.checkpoint_id = checkpoints.parent_checkpoint_id
      AND ps.channel = '__pregel_tasks'
    ORDER BY ps.idx
  ) AS pending_sends
FROM checkpoints
`;

interface CheckpointRow {
  thread_id: string;
  checkpoint_ns: string;
  checkpoint_id: string;
  parent_checkpoint_id: string | null;
  type: string | null;
  checkpoint: Uint8Array | null;
  metadata: Uint8Array | null;
  pending_writes: string;
  pending_sends: string;
}

function copyCheckpoint(checkpoint: Checkpoint): Checkpoint {
  return {
    v: checkpoint.v,
    id: checkpoint.id,
    ts: checkpoint.ts,
    channel_values: { ...checkpoint.channel_values },
    channel_versions: { ...checkpoint.channel_versions },
    versions_seen: Object.fromEntries(
      Object.entries(checkpoint.versions_seen).map(([k, v]) => [k, { ...v }])
    ),
  };
}

export class BunSqliteSaver extends BaseCheckpointSaver {
  private db: Database;
  private isSetup = false;

  constructor(dbPath: string) {
    super();
    this.db = new Database(dbPath);
  }

  static fromConnString(connString: string): BunSqliteSaver {
    return new BunSqliteSaver(connString);
  }

  private setup(): void {
    if (this.isSetup) return;
    this.db.exec(SETUP_SQL);
    this.isSetup = true;
  }

  private async rowToTuple(row: CheckpointRow): Promise<CheckpointTuple> {
    const { thread_id, checkpoint_ns, checkpoint_id } = row;

    const config: RunnableConfig = {
      configurable: {
        thread_id,
        checkpoint_ns,
        checkpoint_id,
      },
    };

    const checkpoint: Checkpoint = (await this.serde.loadsTyped(
      row.type ?? "json",
      row.checkpoint ?? new Uint8Array()
    )) as Checkpoint;

    const metadata: CheckpointMetadata = (await this.serde.loadsTyped(
      row.type ?? "json",
      row.metadata ?? new Uint8Array()
    )) as CheckpointMetadata;

    // Migrate pending sends for v < 4 checkpoints
    if (checkpoint.v < 4) {
      const pendingSends = JSON.parse(row.pending_sends || "[]");
      const validSends = pendingSends.filter(
        (s: { type: string | null }) => s.type !== null
      );
      if (validSends.length > 0) {
        const deserializedSends = await Promise.all(
          validSends.map((s: { type: string; value: string }) =>
            this.serde.loadsTyped(s.type, s.value)
          )
        );
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (checkpoint as any).pending_sends = deserializedSends;
      }
    }

    let parentConfig: RunnableConfig | undefined;
    if (row.parent_checkpoint_id) {
      parentConfig = {
        configurable: {
          thread_id,
          checkpoint_ns,
          checkpoint_id: row.parent_checkpoint_id,
        },
      };
    }

    const pendingWrites: CheckpointPendingWrite[] = [];
    const writes = JSON.parse(row.pending_writes || "[]");
    for (const w of writes) {
      if (w.type === null) continue;
      const value = await this.serde.loadsTyped(w.type, w.value);
      pendingWrites.push([w.task_id, w.channel, value]);
    }

    return {
      config,
      checkpoint,
      metadata,
      parentConfig,
      pendingWrites,
    };
  }

  async getTuple(config: RunnableConfig): Promise<CheckpointTuple | undefined> {
    this.setup();

    const thread_id = config.configurable?.thread_id;
    const checkpoint_ns = config.configurable?.checkpoint_ns ?? "";
    const checkpoint_id = config.configurable?.checkpoint_id;

    if (!thread_id) return undefined;

    let row: CheckpointRow | null;
    if (checkpoint_id) {
      row = this.db
        .query<CheckpointRow, [string, string, string]>(
          `${SELECT_SQL} WHERE thread_id = ? AND checkpoint_ns = ? AND checkpoint_id = ?`
        )
        .get(thread_id, checkpoint_ns, checkpoint_id);
    } else {
      row = this.db
        .query<CheckpointRow, [string, string]>(
          `${SELECT_SQL} WHERE thread_id = ? AND checkpoint_ns = ? ORDER BY checkpoint_id DESC LIMIT 1`
        )
        .get(thread_id, checkpoint_ns);
    }

    if (!row) return undefined;
    return this.rowToTuple(row);
  }

  async *list(
    config: RunnableConfig,
    options?: CheckpointListOptions
  ): AsyncGenerator<CheckpointTuple> {
    this.setup();

    const thread_id = config.configurable?.thread_id;
    const checkpoint_ns = config.configurable?.checkpoint_ns;

    const conditions: string[] = [];
    const params: (string | number)[] = [];

    if (thread_id) {
      conditions.push("thread_id = ?");
      params.push(thread_id);
    }

    if (checkpoint_ns !== undefined && checkpoint_ns !== null) {
      conditions.push("checkpoint_ns = ?");
      params.push(checkpoint_ns);
    }

    if (options?.before?.configurable?.checkpoint_id) {
      conditions.push("checkpoint_id < ?");
      params.push(options.before.configurable.checkpoint_id);
    }

    if (options?.filter) {
      for (const [key, value] of Object.entries(options.filter)) {
        conditions.push(`jsonb(CAST(metadata AS TEXT))->'$.${key}' = ?`);
        params.push(JSON.stringify(value));
      }
    }

    let sql = SELECT_SQL;
    if (conditions.length > 0) {
      sql += ` WHERE ${conditions.join(" AND ")}`;
    }
    sql += " ORDER BY checkpoint_id DESC";

    if (options?.limit) {
      sql += ` LIMIT ?`;
      params.push(options.limit);
    }

    const rows = this.db.query<CheckpointRow, (string | number)[]>(sql).all(...params);

    for (const row of rows) {
      yield await this.rowToTuple(row);
    }
  }

  async put(
    config: RunnableConfig,
    checkpoint: Checkpoint,
    metadata: CheckpointMetadata,
    _newVersions: Record<string, string | number>
  ): Promise<RunnableConfig> {
    this.setup();

    const thread_id = config.configurable?.thread_id;
    const checkpoint_ns = config.configurable?.checkpoint_ns ?? "";
    const parent_checkpoint_id = config.configurable?.checkpoint_id;

    if (!thread_id) {
      throw new Error("thread_id is required in config.configurable");
    }

    const cp = copyCheckpoint(checkpoint);
    const [type, serializedCheckpoint] = await this.serde.dumpsTyped(cp);
    const [, serializedMetadata] = await this.serde.dumpsTyped(metadata);

    this.db
      .query(
        `INSERT OR REPLACE INTO checkpoints
         (thread_id, checkpoint_ns, checkpoint_id, parent_checkpoint_id, type, checkpoint, metadata)
         VALUES (?, ?, ?, ?, ?, ?, ?)`
      )
      .run(
        thread_id,
        checkpoint_ns,
        checkpoint.id,
        parent_checkpoint_id ?? null,
        type,
        serializedCheckpoint,
        serializedMetadata
      );

    return {
      configurable: {
        thread_id,
        checkpoint_ns,
        checkpoint_id: checkpoint.id,
      },
    };
  }

  async putWrites(
    config: RunnableConfig,
    writes: PendingWrite[],
    taskId: string
  ): Promise<void> {
    this.setup();

    const thread_id = config.configurable?.thread_id;
    const checkpoint_ns = config.configurable?.checkpoint_ns ?? "";
    const checkpoint_id = config.configurable?.checkpoint_id;

    if (!thread_id || !checkpoint_id) {
      throw new Error(
        "thread_id and checkpoint_id are required in config.configurable"
      );
    }

    // Pre-serialize all values (async) before entering the sync transaction
    const serialized = await Promise.all(
      writes.map(async ([channel, value]) => {
        const [type, data] = await this.serde.dumpsTyped(value);
        return { channel: channel as string, type, data };
      })
    );

    const insertWrite = this.db.prepare(
      `INSERT OR REPLACE INTO writes
       (thread_id, checkpoint_ns, checkpoint_id, task_id, idx, channel, type, value)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)`
    );

    const insertAll = this.db.transaction(
      (rows: { channel: string; type: string; data: Uint8Array }[]) => {
        for (let idx = 0; idx < rows.length; idx++) {
          const { channel, type, data } = rows[idx]!;
          insertWrite.run(
            thread_id,
            checkpoint_ns,
            checkpoint_id,
            taskId,
            idx,
            channel,
            type,
            data
          );
        }
      }
    );

    insertAll(serialized);
  }

  async deleteThread(threadId: string): Promise<void> {
    this.setup();

    const deleteAll = this.db.transaction((tid: string) => {
      this.db.query("DELETE FROM checkpoints WHERE thread_id = ?").run(tid);
      this.db.query("DELETE FROM writes WHERE thread_id = ?").run(tid);
    });

    deleteAll(threadId);
  }
}
