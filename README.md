<p align="center">
  <img width="768" height="512" alt="github-banner" src="https://github.com/user-attachments/assets/0e731dc1-054a-4660-8592-b9180f203533" />
</p>

<h1 align="center">Inconvo</h1>

<p align="center">Build chat-with-data agents for customer-facing applications</p>

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

## Call agents from your codebase

```typescript
import 'dotenv/config'
import Inconvo from "@inconvoai/node";

const inconvo = new Inconvo({
  apiKey: process.env.INCONVO_API_KEY,
});

const agentConvo = await inconvo.agents.conversations.create(
  "agt_123",
  {
    userIdentifier: "user_123",
    userContext: {
      organisationId: 1
    }
  }
);


const agentResponse = await inconvo.agents.conversations.response.create(agentConvo.id, {
  message: "What is our best selling product this week?",
  stream: false,
});

console.log(agentResponse);

/**
{
  "type": "text",
  "message": "Your most popular product (by total units ordered, to date) is iPhone 15 (Smartphone)."
}
**/
```
