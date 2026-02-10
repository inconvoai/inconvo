"use client";

import {
  Button,
  Stack,
  TextInput,
  Text,
  Group,
  Box,
  Alert,
} from "@mantine/core";
import { useState, useRef, useCallback, useMemo, useEffect } from "react";
import { useForm } from "@mantine/form";
import Editor, { loader } from "@monaco-editor/react";
import { IconInfoCircle } from "@tabler/icons-react";
import {
  type SQLComputedColumnAst,
  type SQLFunction,
  type SQLFunctionName,
  type SQLOperator,
  type SQLColumnReference,
  type SQLValue,
  type SQLOperation,
  type SQLBrackets,
  computedColumnSchema,
  SQLComputedColumnAstSchema,
} from "@repo/types";
import * as monaco from "monaco-editor";
import type { TableSchema, ComputedColumnCreatePayload } from "./types";

if (typeof window !== "undefined") {
  loader.config({ monaco });
}

export interface ComputedColumnFormProps {
  /** The table schema (used for column autocomplete) */
  table: Pick<TableSchema, "id" | "name" | "columns">;
  /** Callback when save is clicked */
  onSave: (payload: ComputedColumnCreatePayload) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save is in progress */
  loading?: boolean;
}

type ComputedColumnFormValues = {
  name: string;
  expression: SQLComputedColumnAst | undefined;
  unit: string | null;
};

// Token types for the parser
type TokenType =
  | "number"
  | "identifier"
  | "operator"
  | "leftParen"
  | "rightParen";
type Token = {
  type: TokenType;
  value: string;
  position: number;
};

const SUPPORTED_FUNCTIONS: SQLFunctionName[] = ["ABS"];

function normalizeFunctionName(value: string): SQLFunctionName | null {
  const normalized = value.toUpperCase();
  return SUPPORTED_FUNCTIONS.includes(normalized as SQLFunctionName)
    ? (normalized as SQLFunctionName)
    : null;
}

function setEditorMarkers(
  editor: monaco.editor.IStandaloneCodeEditor,
  monacoParam: typeof monaco,
  markers: monaco.editor.IMarkerData[],
) {
  const model = editor.getModel();
  if (model) {
    monacoParam.editor.setModelMarkers(model, "owner", markers);
  }
}

// Tokenizer implementation
function tokenizeExpression(expression: string): Token[] {
  const operators = /^(\+|-|\*|\/|%)/;
  const number = /^(\d+(\.\d+)?)/;
  const identifier = /^([a-zA-Z_][\w#]*)/;
  const leftParen = /^(\()/;
  const rightParen = /^(\))/;
  const whitespace = /^\s+/;

  const tokens: Token[] = [];
  let remainingExpression = expression;
  let position = 0;

  while (remainingExpression.length > 0) {
    if (whitespace.test(remainingExpression)) {
      const match = whitespace.exec(remainingExpression);
      if (match?.[0]) {
        const whitespaceLength = match[0].length;
        position += whitespaceLength;
        remainingExpression = remainingExpression.slice(whitespaceLength);
      }
    } else if (leftParen.test(remainingExpression)) {
      tokens.push({ type: "leftParen", value: "(", position });
      position += 1;
      remainingExpression = remainingExpression.slice(1);
    } else if (rightParen.test(remainingExpression)) {
      tokens.push({ type: "rightParen", value: ")", position });
      position += 1;
      remainingExpression = remainingExpression.slice(1);
    } else if (operators.test(remainingExpression)) {
      const match = operators.exec(remainingExpression);
      if (match?.[0]) {
        tokens.push({ type: "operator", value: match[0], position });
        position += match[0].length;
        remainingExpression = remainingExpression.slice(match[0].length);
      }
    } else if (number.test(remainingExpression)) {
      const match = number.exec(remainingExpression);
      if (match?.[0]) {
        tokens.push({ type: "number", value: match[0], position });
        position += match[0].length;
        remainingExpression = remainingExpression.slice(match[0].length);
      }
    } else if (identifier.test(remainingExpression)) {
      const match = identifier.exec(remainingExpression);
      if (match?.[0]) {
        tokens.push({ type: "identifier", value: match[0], position });
        position += match[0].length;
        remainingExpression = remainingExpression.slice(match[0].length);
      }
    } else {
      throw new Error(
        `Unexpected character: ${remainingExpression[0]} at position ${position}`,
      );
    }
  }

  return tokens;
}

function parseExpression(
  tokens: Token[],
  validColumnNames: string[],
): [SQLComputedColumnAst, Token[]] {
  const [left, remainingTokens] = parseTerm(tokens, validColumnNames);
  return parseExpressionTail(left, remainingTokens, validColumnNames);
}

function parseExpressionTail(
  left: SQLComputedColumnAst,
  tokens: Token[],
  validColumnNames: string[],
): [SQLComputedColumnAst, Token[]] {
  if (
    tokens.length === 0 ||
    (tokens[0] && tokens[0].type !== "operator") ||
    (tokens[0] && !["+", "-"].includes(tokens[0].value))
  ) {
    return [left, tokens];
  }

  const operatorToken = tokens[0];
  if (!operatorToken) {
    throw new Error("Expected operator");
  }
  const operator = operatorToken.value as SQLOperator;
  const [right, remainingTokens] = parseTerm(tokens.slice(1), validColumnNames);
  const operation: SQLOperation = {
    type: "operation",
    operator,
    operands: [left, right],
  };
  return parseExpressionTail(operation, remainingTokens, validColumnNames);
}

function parseTerm(
  tokens: Token[],
  validColumnNames: string[],
): [SQLComputedColumnAst, Token[]] {
  const [left, remainingTokens] = parseFactor(tokens, validColumnNames);
  return parseTermTail(left, remainingTokens, validColumnNames);
}

function parseTermTail(
  left: SQLComputedColumnAst,
  tokens: Token[],
  validColumnNames: string[],
): [SQLComputedColumnAst, Token[]] {
  if (
    tokens.length === 0 ||
    (tokens[0] && tokens[0].type !== "operator") ||
    (tokens[0] && !["*", "/", "%"].includes(tokens[0].value))
  ) {
    return [left, tokens];
  }

  const operatorToken = tokens[0];
  if (!operatorToken) {
    throw new Error("Expected operator");
  }
  const operator = operatorToken.value as SQLOperator;
  const [right, remainingTokens] = parseFactor(
    tokens.slice(1),
    validColumnNames,
  );
  const operation: SQLOperation = {
    type: "operation",
    operator,
    operands: [left, right],
  };
  return parseTermTail(operation, remainingTokens, validColumnNames);
}

function parseFactor(
  tokens: Token[],
  validColumnNames: string[],
): [SQLComputedColumnAst, Token[]] {
  const token = tokens[0];
  if (!token) {
    throw new Error("Unexpected end of expression");
  }

  if (token.type === "number") {
    const value: SQLValue = {
      type: "value",
      value: parseFloat(token.value),
    };
    return [value, tokens.slice(1)];
  }

  if (token.type === "identifier") {
    const nextToken = tokens[1];
    if (nextToken?.type === "leftParen") {
      const functionName = normalizeFunctionName(token.value);
      if (!functionName) {
        throw new Error(`Unknown function: ${token.value}`);
      }

      const [arg, afterArg] = parseExpression(tokens.slice(2), validColumnNames);
      if (
        afterArg.length === 0 ||
        (afterArg[0] && afterArg[0].type !== "rightParen")
      ) {
        throw new Error("Missing closing parenthesis");
      }

      const functionNode: SQLFunction = {
        type: "function",
        name: functionName,
        arguments: [arg],
      };
      return [functionNode, afterArg.slice(1)];
    }

    if (!validColumnNames.includes(token.value)) {
      throw new Error(`Unknown column: ${token.value}`);
    }
    const columnRef: SQLColumnReference = {
      type: "column",
      name: token.value,
    };
    return [columnRef, tokens.slice(1)];
  }

  if (token.type === "leftParen") {
    const [expr, afterExpr] = parseExpression(
      tokens.slice(1),
      validColumnNames,
    );
    if (
      afterExpr.length === 0 ||
      (afterExpr[0] && afterExpr[0].type !== "rightParen")
    ) {
      throw new Error("Missing closing parenthesis");
    }
    const brackets: SQLBrackets = {
      type: "brackets",
      expression: expr,
    };
    return [brackets, afterExpr.slice(1)];
  }

  throw new Error(`Unexpected token: ${token?.value}`);
}

function parseExpressionToAst(
  expressionString: string,
  validColumnNames: string[],
): SQLComputedColumnAst {
  const tokens = tokenizeExpression(expressionString);
  if (tokens.length === 0) {
    throw new Error("Expression cannot be empty");
  }
  const [ast, remainingTokens] = parseExpression(tokens, validColumnNames);
  if (remainingTokens[0]) {
    throw new Error(`Unexpected token: ${remainingTokens[0].value}`);
  }
  return ast;
}

export function ComputedColumnForm({
  table,
  onSave,
  onClose,
  loading = false,
}: ComputedColumnFormProps) {
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  const [expressionValue, setExpressionValue] = useState("");
  const [editorInstance, setEditorInstance] =
    useState<monaco.editor.IStandaloneCodeEditor | null>(null);
  const [monacoInstance, setMonacoInstance] = useState<typeof monaco | null>(
    null,
  );
  const [submitError, setSubmitError] = useState<string | null>(null);

  const numberColumns = useMemo(
    () =>
      table.columns
        .filter(({ type, effectiveType }) =>
          ["number", "integer", "bigint", "decimal", "float"].includes(
            effectiveType ?? type,
          ),
        )
        .map((col) => col.name),
    [table.columns],
  );

  const form = useForm<ComputedColumnFormValues>({
    mode: "uncontrolled",
    initialValues: {
      name: "",
      expression: undefined,
      unit: null,
    },
    validate: {
      name: (value) => {
        if (!value) return "Name is required";
        const columnExists = table.columns.some((col) => col.name === value);
        if (columnExists) {
          return "Column with this name already exists in the table";
        }
        return null;
      },
      expression: (value) => {
        try {
          SQLComputedColumnAstSchema.parse(value);
          if (editorInstance && monacoInstance) {
            setEditorMarkers(editorInstance, monacoInstance, []);
          }
          return null;
        } catch (error) {
          if (editorInstance && monacoInstance) {
            const errorMessage =
              error instanceof Error ? error.message : "Invalid expression";
            const model = editorInstance.getModel()!;
            const markers: monaco.editor.IMarkerData[] = [
              {
                severity: monacoInstance.MarkerSeverity.Error,
                message: errorMessage,
                startLineNumber: 1,
                startColumn: 1,
                endLineNumber: model.getLineCount(),
                endColumn: model.getLineMaxColumn(model.getLineCount()),
              },
            ];
            setEditorMarkers(editorInstance, monacoInstance, markers);
          }
          return "Invalid expression";
        }
      },
    },
  });

  // Track registration state at module level
  const languageRegistrationState = useRef({
    languageRegistered: false,
    tokensProviderId: null as monaco.IDisposable | null,
    completionProviderId: null as monaco.IDisposable | null,
  });

  function registerColumnExpressionLanguage(
    monacoParam: typeof monaco,
    columns: string[],
  ) {
    monacoParam.editor.defineTheme("columnExpressionTheme", {
      base: "vs",
      inherit: true,
      rules: [
        { token: "column", foreground: "0000FF", fontStyle: "bold" },
        { token: "operator", foreground: "000000" },
        { token: "number", foreground: "098658" },
        { token: "delimiter", foreground: "000000" },
        { token: "identifier", foreground: "0000FF" },
      ],
      colors: {},
    });
    monacoParam.editor.setTheme("columnExpressionTheme");

    if (!languageRegistrationState.current.languageRegistered) {
      monacoParam.languages.register({ id: "columnExpression" });
      languageRegistrationState.current.languageRegistered = true;
    }

    if (languageRegistrationState.current.tokensProviderId) {
      languageRegistrationState.current.tokensProviderId.dispose();
    }

    const tokensDisposable = monacoParam.languages.setMonarchTokensProvider(
      "columnExpression",
      {
        tokenizer: {
          root: [
            [
              new RegExp(columns.map((col) => `\\b${col}\\b`).join("|")),
              "column",
            ],
            [/\bABS\b/i, "keyword"],
            [/[+\-*/%]+/, "operator"],
            [/\d+(\.\d+)?/, "number"],
            [/[(){}]/, "delimiter"],
            [/[a-zA-Z_]\w*/, "identifier"],
          ],
        },
      },
    );
    languageRegistrationState.current.tokensProviderId = tokensDisposable;

    if (languageRegistrationState.current.completionProviderId) {
      languageRegistrationState.current.completionProviderId.dispose();
    }

    const completionDisposable =
      monacoParam.languages.registerCompletionItemProvider("columnExpression", {
        triggerCharacters: [" ", "+", "-", "*", "/", "%", "(", ")", "."],
        provideCompletionItems: (model, position) => {
          const word = model.getWordUntilPosition(position);
          const range = {
            startLineNumber: position.lineNumber,
            endLineNumber: position.lineNumber,
            startColumn: word.startColumn,
            endColumn: word.endColumn,
          };

          const columnItems = columns.map((col) => ({
            label: col,
            kind: monacoParam.languages.CompletionItemKind.Field,
            detail: "Column",
            insertText: col,
            range: range,
          }));

          const operationItems = [
            { label: "+", detail: "Addition" },
            { label: "-", detail: "Subtraction" },
            { label: "*", detail: "Multiplication" },
            { label: "/", detail: "Division" },
            { label: "%", detail: "Modulo" },
          ].map((op) => ({
            label: op.label,
            kind: monacoParam.languages.CompletionItemKind.Operator,
            detail: op.detail,
            insertText: op.label,
            range: range,
          }));

          const functionItems = [
            {
              label: "ABS",
              detail: "Absolute value",
              insertText: "ABS(${1:column})",
            },
          ].map((fn) => ({
            label: fn.label,
            kind: monacoParam.languages.CompletionItemKind.Function,
            detail: fn.detail,
            insertText: fn.insertText,
            insertTextRules:
              monacoParam.languages.CompletionItemInsertTextRule.InsertAsSnippet,
            range: range,
          }));

          const numberSuggestion = {
            label: "123",
            kind: monacoParam.languages.CompletionItemKind.Value,
            detail: "Number value",
            insertText: "123",
            range: range,
          };

          return {
            suggestions: [
              ...functionItems,
              ...operationItems,
              numberSuggestion,
              ...columnItems,
            ],
          };
        },
      });

    languageRegistrationState.current.completionProviderId =
      completionDisposable;
  }

  const handleEditorDidMount = useCallback(
    (
      editor: monaco.editor.IStandaloneCodeEditor,
      monacoParam: typeof monaco,
    ): void => {
      editorRef.current = editor;
      setEditorInstance(editor);
      setMonacoInstance(monacoParam);
      registerColumnExpressionLanguage(monacoParam, numberColumns);

      editor.onDidFocusEditorWidget(() => {
        setTimeout(() => {
          editor.trigger("keyboard", "editor.action.triggerSuggest", {});
        }, 100);
      });
    },
    [numberColumns],
  );

  useEffect(() => {
    const registrationState = languageRegistrationState.current;
    return () => {
      if (registrationState.tokensProviderId) {
        registrationState.tokensProviderId.dispose();
      }
      if (registrationState.completionProviderId) {
        registrationState.completionProviderId.dispose();
      }
    };
  }, []);

  const handleSubmit = form.onSubmit(async (values) => {
    setSubmitError(null);
    try {
      if (values.expression) {
        const validatedColumn = computedColumnSchema.parse({
          type: "number" as const,
          name: values.name,
          unit: values.unit,
          table: {
            name: table.name,
          },
          ast: values.expression,
        });
        await onSave({
          name: validatedColumn.name,
          ast: validatedColumn.ast,
          unit: validatedColumn.unit,
        });
      } else {
        form.setFieldError("expression", "Expression is required");
      }
    } catch (error) {
      const errorMessage =
        error instanceof Error ? error.message : "Invalid expression";
      if (errorMessage.includes("Unique constraint failed")) {
        form.setFieldError(
          "name",
          "Computed column with this name already exists",
        );
      } else {
        setSubmitError(errorMessage);
      }
    }
  });

  return (
    <Stack h="100%">
      <Alert
        icon={<IconInfoCircle size={16} />}
        title="Computed Columns"
        color="blue"
        variant="light"
      >
        <Text size="sm">
          Create derived columns using mathematical expressions with existing
          numeric columns. Supports ABS(...), operators: +, -, *, /, % and
          parentheses for grouping.
        </Text>
      </Alert>

      {submitError && (
        <Alert color="red" icon={<IconInfoCircle size={16} />}>
          {submitError}
        </Alert>
      )}

      <form onSubmit={handleSubmit}>
        <Stack gap="md">
          <TextInput
            withAsterisk
            label="Column Name"
            placeholder="e.g., total_price, avg_cost"
            key={form.key("name")}
            disabled={loading}
            {...form.getInputProps("name")}
          />

          <TextInput
            label="Unit"
            placeholder="e.g., USD, kg, miles"
            key={form.key("unit")}
            disabled={loading}
            {...form.getInputProps("unit")}
          />

          <Stack gap="xs">
            <Text size="sm" fw={500}>
              Expression{" "}
              <span style={{ color: "var(--mantine-color-red-6)" }}>*</span>
            </Text>
            <Box
              style={{
                height: "50px",
                border: "1px solid var(--mantine-color-gray-3)",
                borderRadius: "var(--mantine-radius-sm)",
              }}
            >
              <Editor
                height="100%"
                defaultLanguage="columnExpression"
                defaultValue=""
                value={expressionValue}
                onChange={(value) => {
                  const newValue = value ?? "";
                  setExpressionValue(newValue);

                  try {
                    if (newValue) {
                      const parsedAst = parseExpressionToAst(
                        newValue,
                        numberColumns,
                      );
                      form.setFieldValue("expression", parsedAst);
                      if (editorInstance && monacoInstance) {
                        setEditorMarkers(editorInstance, monacoInstance, []);
                      }
                    } else {
                      form.setFieldValue("expression", undefined);
                    }
                  } catch (error) {
                    form.setFieldValue("expression", undefined);
                    if (editorInstance && monacoInstance) {
                      const errorMessage =
                        error instanceof Error
                          ? error.message
                          : "Invalid expression";
                      let startLineNumber = 1;
                      let startColumn = 1;
                      const model = editorInstance.getModel();
                      if (model) {
                        let endLineNumber = model.getLineCount();
                        let endColumn = model.getLineMaxColumn(
                          model.getLineCount(),
                        );

                        const errorStartMatch = /at position (\d+)/.exec(
                          errorMessage,
                        );
                        if (errorStartMatch?.[1]) {
                          const errorPosition = parseInt(
                            errorStartMatch[1],
                            10,
                          );
                          const lines = model.getLinesContent();
                          let position = 0;
                          for (let i = 0; i < lines.length; i++) {
                            if (position + lines[i]!.length >= errorPosition) {
                              startLineNumber = i + 1;
                              startColumn = errorPosition - position + 1;
                              endLineNumber = startLineNumber;
                              endColumn = startColumn;
                              break;
                            }
                            position += lines[i]!.length + 1;
                          }
                        }

                        const tokenMatch = /Unexpected token: (\S+)/.exec(
                          errorMessage,
                        );
                        if (tokenMatch?.[1]) {
                          const unexpectedToken = tokenMatch[1];
                          const content = model.getValue();
                          const tokenIndex = content.indexOf(unexpectedToken);
                          if (tokenIndex !== -1) {
                            const lines = content.split("\n");
                            let position = 0;
                            for (let i = 0; i < lines.length; i++) {
                              if (position + lines[i]!.length >= tokenIndex) {
                                startLineNumber = i + 1;
                                startColumn = tokenIndex - position + 1;
                                endLineNumber = startLineNumber;
                                endColumn =
                                  startColumn + unexpectedToken.length;
                                break;
                              }
                              position += lines[i]!.length + 1;
                            }
                          }
                        }

                        setEditorMarkers(editorInstance, monacoInstance, [
                          {
                            startLineNumber,
                            startColumn,
                            endLineNumber,
                            endColumn,
                            message: errorMessage,
                            severity: monacoInstance.MarkerSeverity.Error,
                          },
                        ]);
                      }
                    }
                  }
                }}
                onMount={handleEditorDidMount}
                options={{
                  minimap: { enabled: false },
                  fontSize: 14,
                  scrollBeyondLastLine: false,
                  wordWrap: "on",
                  lineNumbers: "off",
                  glyphMargin: false,
                  folding: false,
                  lineDecorationsWidth: 0,
                  lineNumbersMinChars: 0,
                  renderLineHighlight: "none",
                  scrollbar: {
                    vertical: "hidden",
                    horizontal: "hidden",
                  },
                  readOnly: loading,
                }}
              />
            </Box>
            {form.errors.expression && (
              <Text c="red" size="sm">
                {form.errors.expression}
              </Text>
            )}
          </Stack>

          <Group justify="flex-end" mt="md">
            <Button variant="outline" onClick={onClose} disabled={loading}>
              Cancel
            </Button>
            <Button type="submit" loading={loading}>
              Create Column
            </Button>
          </Group>
        </Stack>
      </form>
    </Stack>
  );
}
