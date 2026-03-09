import type { JoinPathHop } from "@repo/types";

export interface JoinGuidanceOption {
  name: string;
  table: string;
  path: JoinPathHop[];
}

export interface JoinGuidanceInput {
  table: string;
  name?: string;
  path: JoinPathHop[];
}

export function formatJoinPath(path: JoinPathHop[]) {
  return path
    .map((hop) => `[${hop.source.join(", ")}] -> [${hop.target.join(", ")}]`)
    .join(" | ");
}

export function buildJoinDescriptorDescription(
  intro: string,
  joinOptions: JoinGuidanceOption[],
  options?: {
    emptyStateMessage?: string;
    nameRequirement?: "optional" | "required";
  },
) {
  if (joinOptions.length === 0) {
    return options?.emptyStateMessage ?? `${intro} No joins available.`;
  }

  const nameRequirement = options?.nameRequirement ?? "optional";
  const example = joinOptions[0]!;
  const rule =
    "`table` = target table in parentheses, `name` = value before parentheses, `path` = exact listed path.";
  const exampleLine =
    nameRequirement === "required"
      ? `Example: ${example.name} (${example.table}) -> table: "${example.table}", name: "${example.name}".`
      : `Example: ${example.name} (${example.table}) -> table: "${example.table}", name: "${example.name}". Omit name to use the name shown before parentheses.`;

  return [
    intro,
    rule,
    exampleLine,
    "Available joins:",
    ...joinOptions.map(
      (option) =>
        `${option.name} (${option.table}) path=${formatJoinPath(option.path)}`,
    ),
  ].join("\n");
}

export function buildJoinPathMismatchMessage(
  join: JoinGuidanceInput,
  joinOptions: JoinGuidanceOption[],
  preferredOption?: JoinGuidanceOption,
) {
  const likelyOption =
    preferredOption ?? findLikelyJoinOption(join, joinOptions);
  if (!likelyOption) {
    return "Join path does not match any available relation path.";
  }

  return `Join path does not match the available relation path for ${likelyOption.name} (${likelyOption.table}). Use path=${formatJoinPath(likelyOption.path)} exactly.`;
}

export function buildJoinTableMismatchMessage(
  join: Pick<JoinGuidanceInput, "table" | "name">,
  option: Pick<JoinGuidanceOption, "name" | "table">,
  context: string,
) {
  const baseMessage = `Join table must be ${option.table} for ${context}.`;

  if (join.table === option.name) {
    return `${baseMessage} You passed the join name in \`table\`; use \`table: "${option.table}"\` and \`name: "${option.name}"\` instead.`;
  }

  if (join.table.includes(".")) {
    return `${baseMessage} \`table\` must be the target table name, not a dotted \`name\` value. Use \`table: "${option.table}"\` and \`name: "${option.name}"\` if you want to keep the name explicit.`;
  }

  return baseMessage;
}

function findLikelyJoinOption(
  join: JoinGuidanceInput,
  joinOptions: JoinGuidanceOption[],
) {
  if (joinOptions.length === 1) {
    return joinOptions[0]!;
  }

  const nameTokens = [join.name, join.table].filter(
    (value): value is string => typeof value === "string" && value.length > 0,
  );

  const nameMatches = joinOptions.filter((option) =>
    nameTokens.some(
      (token) => option.name === token || option.name.endsWith(`.${token}`),
    ),
  );
  if (nameMatches.length === 1) {
    return nameMatches[0]!;
  }

  const tableMatches = joinOptions.filter(
    (option) => option.table === join.table,
  );
  if (tableMatches.length === 1) {
    return tableMatches[0]!;
  }

  return null;
}
