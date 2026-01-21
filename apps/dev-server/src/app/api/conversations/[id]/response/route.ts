import { type NextRequest } from "next/server";
import { v4 as uuidv4 } from "uuid";
import { inconvoAgent } from "@repo/agents";
import { getConnector } from "~/lib/connector";
import { getCheckpointer } from "~/lib/checkpointer";
import { getSchema } from "~/lib/schema";
import { getConversation, updateConversation } from "~/lib/conversations";
import type { InconvoResponse } from "@repo/types";

const DEV_ORGANISATION_ID = "dev-org";
const DEV_AGENT_ID = "dev-agent";

interface ResponseCreateParams {
  message: string;
  stream?: boolean;
}

interface SDKResponse {
  id: string;
  conversationId: string;
  type: string;
  message: string;
  chart?: unknown;
  table?: unknown;
}

interface StreamEvent {
  type:
    | "response.created"
    | "response.progress"
    | "response.completed"
    | "response.error";
  id: string;
  message?: string;
  response?: SDKResponse;
  error?: string;
}

// POST /api/conversations/[id]/response - Create a response (send a message)
export async function POST(
  request: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  try {
    const { id: conversationId } = await params;
    const body = (await request.json()) as ResponseCreateParams;
    const { message, stream = true } = body;

    if (!message) {
      return new Response(JSON.stringify({ error: "Message is required" }), {
        status: 400,
        headers: { "Content-Type": "application/json" },
      });
    }

    // Get existing conversation
    const conversation = getConversation(conversationId);
    if (!conversation) {
      return new Response(JSON.stringify({ error: "Conversation not found" }), {
        status: 404,
        headers: { "Content-Type": "application/json" },
      });
    }

    const runId = uuidv4();
    const schema = await getSchema();
    const connector = getConnector();
    const checkpointer = getCheckpointer();

    // Create the agent graph with databases array (single database for dev-server)
    const { graph } = await inconvoAgent({
      databases: [
        {
          friendlyName: "Development Database",
          context: null,
          schema,
          connector,
        },
      ],
      checkpointer,
      conversation,
      orgId: DEV_ORGANISATION_ID,
      agentId: DEV_AGENT_ID,
      runId,
      userIdentifier: conversation.userIdentifier,
    });

    // Helper to format response for SDK
    const formatResponseForSDK = (response: InconvoResponse): SDKResponse => {
      const base = {
        id: runId,
        conversationId,
        type: response.type,
        message: response.message,
      };
      if (response.type === "chart") {
        return { ...base, chart: response.spec };
      }
      if (response.type === "table") {
        return { ...base, table: response.table };
      }
      return base;
    };

    // Non-streaming response
    if (!stream) {
      try {
        const eventStream = graph.streamEvents(
          {
            userQuestion: message,
            userContext: conversation.userContext ?? {},
            runId,
          },
          {
            configurable: { thread_id: conversationId },
            recursionLimit: 32,
            runId,
            runName: "inconvo-agent",
            version: "v2",
          },
        );

        let lastResponse: InconvoResponse | undefined;

        for await (const event of eventStream) {
          if (
            event.event === "on_chain_end" &&
            event.name === "format_response"
          ) {
            const output = event.data?.output as
              | { answer?: InconvoResponse }
              | undefined;
            if (output?.answer) {
              lastResponse = output.answer;
            }
          }
        }

        if (lastResponse) {
          // Update conversation title if first message
          if (!conversation.title && lastResponse.type === "text") {
            updateConversation(conversationId, {
              title: message.slice(0, 100),
            });
          }

          return new Response(
            JSON.stringify(formatResponseForSDK(lastResponse)),
            {
              headers: { "Content-Type": "application/json" },
            },
          );
        }

        return new Response(
          JSON.stringify({ error: "No response generated" }),
          {
            status: 500,
            headers: { "Content-Type": "application/json" },
          },
        );
      } catch (error) {
        console.error("[/api/conversations/[id]/response] Agent error:", error);
        return new Response(
          JSON.stringify({
            error:
              error instanceof Error ? error.message : "Agent execution failed",
          }),
          {
            status: 500,
            headers: { "Content-Type": "application/json" },
          },
        );
      }
    }

    // Streaming response
    const encoder = new TextEncoder();

    const responseStream = new ReadableStream({
      async start(controller) {
        const sendEvent = (event: StreamEvent) => {
          controller.enqueue(
            encoder.encode(`data: ${JSON.stringify(event)}\n\n`),
          );
        };

        try {
          // Send created event
          sendEvent({
            type: "response.created",
            id: runId,
          });

          // Stream events from the agent
          const eventStream = graph.streamEvents(
            {
              userQuestion: message,
              userContext: conversation.userContext ?? {},
              runId,
            },
            {
              configurable: { thread_id: conversationId },
              recursionLimit: 32,
              runId,
              runName: "inconvo-agent",
              version: "v2",
            },
          );

          let lastResponse: InconvoResponse | undefined;

          for await (const event of eventStream) {
            // Handle chain end events
            if (event.event === "on_chain_end") {
              // Progress updates from inconvo_agent node
              if (event.name === "inconvo_agent") {
                const output = event.data?.output as
                  | { messages?: unknown }
                  | undefined;
                if (output?.messages) {
                  const messages = Array.isArray(output.messages)
                    ? (output.messages as Array<{
                        _getType?: () => string;
                        content?: unknown;
                      }>)
                    : [
                        output.messages as {
                          _getType?: () => string;
                          content?: unknown;
                        },
                      ];
                  for (const msg of messages) {
                    if (msg?._getType?.() === "ai" && msg.content) {
                      const content =
                        typeof msg.content === "string"
                          ? msg.content
                          : JSON.stringify(msg.content);
                      // Only send non-empty, non-JSON content as progress
                      if (
                        content &&
                        !content.startsWith("{") &&
                        !content.startsWith("[")
                      ) {
                        sendEvent({
                          type: "response.progress",
                          id: runId,
                          message: content,
                        });
                      }
                    }
                  }
                }
              }

              // Final response from format_response node
              if (event.name === "format_response") {
                const output = event.data?.output as
                  | { answer?: InconvoResponse }
                  | undefined;
                if (output?.answer) {
                  lastResponse = output.answer;
                }
              }
            }
          }

          // Send completed event with final response
          if (lastResponse) {
            sendEvent({
              type: "response.completed",
              id: runId,
              response: formatResponseForSDK(lastResponse),
            });

            // Update conversation title if first message
            if (!conversation.title && lastResponse.type === "text") {
              updateConversation(conversationId, {
                title: message.slice(0, 100),
              });
            }
          } else {
            sendEvent({
              type: "response.error",
              id: runId,
              error: "No response generated",
            });
          }
        } catch (error) {
          console.error(
            "[/api/conversations/[id]/response] Agent error:",
            error,
          );
          sendEvent({
            type: "response.error",
            id: runId,
            error:
              error instanceof Error ? error.message : "Agent execution failed",
          });
        } finally {
          controller.close();
        }
      },
    });

    return new Response(responseStream, {
      headers: {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      },
    });
  } catch (error) {
    console.error("[/api/conversations/[id]/response] Request error:", error);
    return new Response(
      JSON.stringify({
        error: error instanceof Error ? error.message : "Request failed",
      }),
      {
        status: 500,
        headers: { "Content-Type": "application/json" },
      },
    );
  }
}
