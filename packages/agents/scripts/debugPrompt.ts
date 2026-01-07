#!/usr/bin/env npx tsx
/// <reference types="node" />

import * as fs from "node:fs";
import * as path from "node:path";
import * as hub from "langchain/hub";
import type { ChatPromptTemplate } from "@langchain/core/prompts";

// All prompts used in the codebase
const KNOWN_PROMPTS = [
  "inconvo_agent_gpt5_dev:144f4952",
  "select_table:dbe22856",
  "select_operation:e74c537d",
  "where_condition_agent_5:dded782c",
  "extend_query:4836f794",
  "generate_conversation_title:9a82e721",
] as const;

// Load .env file from apps/platform
function loadEnv() {
  const scriptDir = import.meta.dirname;
  const envPath = path.resolve(scriptDir, "../../../apps/platform/.env");

  if (fs.existsSync(envPath)) {
    console.log(`Loading env from: ${envPath}\n`);
    const content = fs.readFileSync(envPath, "utf-8");
    for (const line of content.split("\n")) {
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith("#")) continue;
      const eqIndex = trimmed.indexOf("=");
      if (eqIndex === -1) continue;
      const key = trimmed.slice(0, eqIndex);
      let value = trimmed.slice(eqIndex + 1);
      // Remove surrounding quotes
      if (
        (value.startsWith('"') && value.endsWith('"')) ||
        (value.startsWith("'") && value.endsWith("'"))
      ) {
        value = value.slice(1, -1);
      }
      if (!process.env[key]) {
        process.env[key] = value;
      }
    }
    return true;
  }
  return false;
}

async function fetchPrompt(promptName: string) {
  console.log(`\n${"=".repeat(60)}`);
  console.log(`Fetching: ${promptName}`);
  console.log("=".repeat(60));

  try {
    const prompt = (await hub.pull(promptName)) as ChatPromptTemplate;

    // Display prompt messages
    if (prompt.promptMessages) {
      for (const message of prompt.promptMessages) {
        const role =
          message.constructor.name.replace("MessagePromptTemplate", "") ||
          "Unknown";
        console.log(`\n[${role}]`);

        if ("prompt" in message && message.prompt) {
          const template =
            (message.prompt as { template?: string }).template || "";
          console.log(template);
        } else if ("content" in message) {
          console.log(message.content);
        }
      }
    }

    // Display input variables
    if (prompt.inputVariables?.length) {
      console.log(`\n[Input Variables]`);
      console.log(prompt.inputVariables.join(", "));
    }

    return true;
  } catch (error) {
    console.error(`Error fetching prompt: ${error}`);
    return false;
  }
}

function printUsage() {
  console.log("Debug LangChain Hub Prompts\n");
  console.log("Usage:");
  console.log("  pnpm debug:prompt <prompt-name>    Fetch a specific prompt");
  console.log("  pnpm debug:prompt --all            Fetch all known prompts");
  console.log("  pnpm debug:prompt --list           List known prompts\n");
  console.log("Environment:");
  console.log("  LANGCHAIN_API_KEY loaded from apps/platform/.env\n");
  console.log("Examples:");
  console.log("  pnpm debug:prompt inconvo/select_table:dbe22856");
  console.log("  pnpm debug:prompt --all\n");
}

async function main() {
  const args = process.argv.slice(2);

  if (args.includes("--help") || args.includes("-h")) {
    printUsage();
    return;
  }

  if (args.length === 0 || args[0] === "--list") {
    console.log("Known prompts in codebase:");
    for (const p of KNOWN_PROMPTS) {
      console.log(`  - ${p}`);
    }
    console.log("\nRun with --help for usage info");
    return;
  }

  // Load env from apps/platform/.env
  loadEnv();

  if (!process.env.LANGCHAIN_API_KEY) {
    console.error("Error: LANGCHAIN_API_KEY not found in apps/platform/.env\n");
    process.exit(1);
  }

  if (args[0] === "--all") {
    for (const promptName of KNOWN_PROMPTS) {
      await fetchPrompt(promptName);
    }
  } else {
    const success = await fetchPrompt(args[0]);
    if (!success) {
      console.log("\nHints:");
      console.log(
        "  - Prompt names usually need an owner prefix: owner/prompt_name:version",
      );
      console.log(
        "  - Check if your LANGCHAIN_API_KEY has access to this prompt",
      );
    }
  }
}

main().catch(console.error);
