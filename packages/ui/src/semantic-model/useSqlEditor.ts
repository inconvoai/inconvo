import { useEffect, useRef } from "react";
import * as monaco from "monaco-editor";
import type { TableWithColumns, VirtualTableDialect } from "./types";

const BASE_SQL_KEYWORDS = [
  "SELECT", "FROM", "WHERE", "JOIN", "LEFT JOIN", "RIGHT JOIN",
  "INNER JOIN", "OUTER JOIN", "FULL JOIN", "CROSS JOIN",
  "ON", "AND", "OR", "NOT", "IN", "EXISTS", "BETWEEN",
  "LIKE", "IS NULL", "IS NOT NULL", "AS", "DISTINCT",
  "GROUP BY", "ORDER BY", "HAVING", "LIMIT", "OFFSET",
  "UNION", "UNION ALL",
  "CASE", "WHEN", "THEN", "ELSE", "END",
  "COUNT", "SUM", "AVG", "MIN", "MAX",
  "COALESCE", "NULLIF", "CAST",
  "ASC", "DESC", "WITH",
];

const DIALECT_KEYWORDS: Record<string, string[]> = {
  postgresql: [
    "ILIKE", "SIMILAR TO", "LATERAL", "RETURNING",
    "EXTRACT", "GENERATE_SERIES", "ARRAY_AGG", "STRING_AGG",
    "ROW_NUMBER", "RANK", "DENSE_RANK", "OVER", "PARTITION BY",
    "FILTER", "WITHIN GROUP", "MATERIALIZED",
  ],
  mysql: [
    "IFNULL", "GROUP_CONCAT", "SEPARATOR",
    "ROW_NUMBER", "RANK", "DENSE_RANK", "OVER", "PARTITION BY",
    "STRAIGHT_JOIN",
  ],
  bigquery: [
    "UNNEST", "STRUCT", "ARRAY", "SAFE_CAST",
    "IFNULL", "IF", "SAFE_DIVIDE",
    "ROW_NUMBER", "RANK", "DENSE_RANK", "OVER", "PARTITION BY",
    "QUALIFY", "PIVOT", "UNPIVOT",
    "EXCEPT", "REPLACE",
  ],
  redshift: [
    "ILIKE", "GETDATE", "DATEADD", "DATEDIFF", "NVL",
    "LISTAGG", "APPROXIMATE",
    "ROW_NUMBER", "RANK", "DENSE_RANK", "OVER", "PARTITION BY",
    "DISTKEY", "SORTKEY",
  ],
  mssql: [
    "TOP", "ISNULL", "GETDATE", "DATEADD", "DATEDIFF",
    "STRING_AGG", "CROSS APPLY", "OUTER APPLY",
    "ROW_NUMBER", "RANK", "DENSE_RANK", "OVER", "PARTITION BY",
    "PIVOT", "UNPIVOT",
  ],
};

const SQL_EDITOR_THEME: monaco.editor.IStandaloneThemeData = {
  base: "vs",
  inherit: true,
  rules: [],
  colors: {
    "editorLineNumber.foreground": "#9e9e9e",
    "editorGutter.background": "#f5f5f5",
  },
};

function buildCompletionProvider(
  dialect: VirtualTableDialect | undefined,
  availableTablesRef: React.RefObject<TableWithColumns[]>,
  monacoInstance: typeof monaco,
): monaco.languages.CompletionItemProvider {
  const sqlKeywords = [
    ...BASE_SQL_KEYWORDS,
    ...(dialect ? DIALECT_KEYWORDS[dialect] ?? [] : []),
  ];

  return {
    triggerCharacters: [" ", ".", ","],
    provideCompletionItems: (model: monaco.editor.ITextModel, position: monaco.Position) => {
      const word = model.getWordUntilPosition(position);
      const range = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      };

      // Exclude virtual tables — they can't be joined in SQL
      const physicalTables = availableTablesRef.current.filter(
        (t) => t.source !== "VIRTUAL",
      );

      // Check if we're after a dot (table.column context)
      const textBeforeCursor = model.getValueInRange({
        startLineNumber: position.lineNumber,
        startColumn: 1,
        endLineNumber: position.lineNumber,
        endColumn: word.startColumn,
      });
      const dotMatch = textBeforeCursor.match(/(\w+)\.\s*$/);

      if (dotMatch?.[1]) {
        // After a dot — suggest columns for that table
        const tableName = dotMatch[1].toLowerCase();
        const matchedTable = physicalTables.find(
          (t) => t.name.toLowerCase() === tableName,
        );
        if (matchedTable) {
          return {
            suggestions: matchedTable.columns.map((col) => ({
              label: col.name,
              kind: monacoInstance.languages.CompletionItemKind.Field,
              detail: col.type ? `${matchedTable.name}.${col.name} (${col.type})` : `${matchedTable.name}.${col.name}`,
              insertText: col.name,
              range,
            })),
          };
        }
      }

      // Table suggestions
      const tableSuggestions = physicalTables.map((t) => ({
        label: t.name,
        kind: monacoInstance.languages.CompletionItemKind.Class,
        detail: `Table (${t.columns.length} columns)`,
        insertText: t.name,
        range,
      }));

      // Column suggestions, deduplicated by name
      const seenColumns = new Map<string, { tableName: string; type?: string }>();
      for (const t of physicalTables) {
        for (const col of t.columns) {
          if (!seenColumns.has(col.name)) {
            seenColumns.set(col.name, { tableName: t.name, type: col.type });
          }
        }
      }
      const columnSuggestions = Array.from(seenColumns.entries()).map(
        ([name, { tableName, type }]) => ({
          label: name,
          kind: monacoInstance.languages.CompletionItemKind.Field,
          detail: type ? `${tableName} (${type})` : tableName,
          insertText: name,
          range,
        }),
      );

      // SQL keyword suggestions
      const keywordSuggestions = sqlKeywords.map((kw) => ({
        label: kw,
        kind: monacoInstance.languages.CompletionItemKind.Keyword,
        detail: "SQL",
        insertText: kw,
        range,
      }));

      return {
        suggestions: [
          ...tableSuggestions,
          ...columnSuggestions,
          ...keywordSuggestions,
        ],
      };
    },
  };
}

interface UseSqlEditorOptions {
  dialect: VirtualTableDialect | undefined;
  availableTables: TableWithColumns[];
  onSave: () => void;
}

export function useSqlEditor({ dialect, availableTables, onSave }: UseSqlEditorOptions) {
  const sqlCompletionDisposable = useRef<monaco.IDisposable | null>(null);
  const availableTablesRef = useRef<TableWithColumns[]>(availableTables);
  const handleSaveRef = useRef<() => void>(onSave);

  useEffect(() => {
    availableTablesRef.current = availableTables;
  }, [availableTables]);

  useEffect(() => {
    handleSaveRef.current = onSave;
  }, [onSave]);

  // Cleanup SQL completion provider on unmount
  useEffect(() => {
    return () => {
      if (sqlCompletionDisposable.current) {
        sqlCompletionDisposable.current.dispose();
      }
    };
  }, []);

  const beforeMount = (monacoInstance: typeof monaco) => {
    monacoInstance.editor.defineTheme("sqlEditorTheme", SQL_EDITOR_THEME);

    // Dispose previous completion provider if any
    if (sqlCompletionDisposable.current) {
      sqlCompletionDisposable.current.dispose();
    }

    sqlCompletionDisposable.current =
      monacoInstance.languages.registerCompletionItemProvider(
        "sql",
        buildCompletionProvider(dialect, availableTablesRef, monacoInstance),
      );
  };

  const onMount = (editor: monaco.editor.IStandaloneCodeEditor) => {
    editor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
      () => {
        handleSaveRef.current();
      },
    );
  };

  return { beforeMount, onMount };
}
