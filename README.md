[![Inconvo](https://github.com/inconvoai/.github/raw/main/profile/inconvo-banner.png)](https://inconvo.com)

<h1 align="center">Inconvo</h1>

<p align="center"> 💬📊 Build chat-with-data agents for customer-facing applications 📊💬 </p>

<div align="center">

[Website](https://inconvo.com) | [Docs](https://inconvo.com/docs) | [Issues](https://github.com/inconvoai/inconvo/issues)

[![Discord](https://img.shields.io/badge/Join%20Community-5865F2?logo=discord&logoColor=white)](https://discord.gg/aFsbY9Uf9C)
[![Open Source](https://img.shields.io/badge/Open%20Source-%E2%9D%A4-red.svg)](https://github.com/inconvoai/inconvo)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](https://github.com/inconvoai/inconvo/blob/main/LICENSE)
[![Cloud](https://img.shields.io/badge/Cloud-☁️-blue)](https://app.inconvo.ai)
[![Y Combinator](https://img.shields.io/badge/Y%20Combinator-S23-orange)](https://www.ycombinator.com/companies/inconvo)
[![Documentation](https://img.shields.io/badge/Documentation-📕-blue)](https://inconvo.com/docs)

</div>

## About Inconvo

Inconvo is the open-source platform for building data agents on production data. With safe queries, permissions, and structured outputs.

A data agent is a service your application calls to answer natural-language questions over live production data — safely, with enforced permissions, and in structured outputs your software can rely on.

## The platform designed for building data agents

Use Inconvo to build and deploy data agents that query real production databases and can be called directly from your application. You get safe query execution, permissions, stateful conversations, and observability out of the box.

- **Safe queries:** All generated queries are validated and constrained to explicitly allowed tables, columns, and joins before execution.
- **Permissions & multi-tenancy:** Enforce row-, table-, and column-level access automatically. Apply tenant context at runtime without custom query logic.
- **Stateful interactions:** Agents retain filters and refinements across turns without manual state management.
- **Observability & monitoring:** Every run is traceable. Inspect generated queries, execution logs, and failures to understand exactly what happened.
- **Semantic modeling:** Start querying immediately, then layer in business context—metrics, terminology, computed fields, join rules—over time.

## Demo

https://github.com/user-attachments/assets/cbb468a6-3711-4ad1-92d6-d3871456ae08

## Call agents from your codebase

```typescript
import "dotenv/config";
import Inconvo from "@inconvoai/node";

const inconvo = new Inconvo({
  apiKey: process.env.INCONVO_API_KEY,
});

const agentConvo = await inconvo.agents.conversations.create("agt_123", {
  userIdentifier: "user_123",
  userContext: {
    organisationId: 1,
  },
});

const agentResponse = await inconvo.agents.conversations.response.create(
  agentConvo.id,
  {
    message: "What is our best selling product this week?",
    stream: false,
  },
);

console.log(agentResponse);

/**
{
  "type": "text",
  "message": "Your most popular product (by total units ordered, to date) is iPhone 15 (Smartphone)."
}
**/
```

## Getting started with Inconvo

### Inconvo Cloud

The fastest and most reliable way to get started with Inconvo is signing up for free to [Inconvo Cloud](https://app.inconvo.ai), and following the onboarding instructions.

Useful links:
- [Quick start](https://inconvo.com/docs/getting-started/quickstart/) - get up and running in minutes

### Run Inconvo locally

Run Inconvo on your own machine, perfect for evaluation.

```bash
npx inconvo@latest dev
```

Open the dashboard at http://localhost:26686




