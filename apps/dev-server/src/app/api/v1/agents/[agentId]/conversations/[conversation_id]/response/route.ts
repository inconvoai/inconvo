import { type NextRequest, NextResponse } from "next/server";
import { v4 as uuidv4 } from "uuid";
import { inconvoAgent } from "@repo/agents";
import { getConnector } from "~/lib/connector";
import { getCheckpointer } from "~/lib/checkpointer";
import { getSchema } from "~/lib/schema";
import {
  getConversation,
  updateConversation,
  appendMessages,
  type ChatMessage,
} from "~/lib/conversations";
import type { InconvoResponse } from "@repo/types";
import { DEV_AGENT_ID, DEV_ORGANISATION_ID } from "~/lib/constants";
import { corsHeaders, handleOptions } from "~/lib/cors";
import { getPostHogClient } from "~/lib/posthog-server";
import { trackResponsePerformance } from "~/lib/telemetry";

interface SDKResponse {
  id: string;
  conversationId: string;
  type: string;
  message: string;
  spec?: unknown;
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

// Handle OPTIONS preflight
export async function OPTIONS() {
  return handleOptions();
}

// POST /api/v1/agents/{agentId}/conversations/{conversation_id}/response - Create a response (send a message)
export async function POST(
  request: NextRequest,
  { params }: { params: Promise<{ agentId: string; conversation_id: string }> },
) {
  try {
    const { agentId, conversation_id: conversationId } = await params;

    // Validate agent ID
    if (agentId !== DEV_AGENT_ID) {
      return NextResponse.json(
        { error: "Invalid agent ID" },
        { status: 404, headers: corsHeaders },
      );
    }

    // Parse request body
    const body = (await request.json()) as {
      message?: string;
      stream?: boolean;
    };

    const { message, stream = true } = body;

    if (!message || typeof message !== "string") {
      return NextResponse.json(
        { error: "message is required and must be a non-empty string" },
        { status: 400, headers: corsHeaders },
      );
    }

    // Get existing conversation
    const conversation = await getConversation(conversationId);
    if (!conversation) {
      return NextResponse.json(
        { error: "Conversation not found" },
        { status: 404, headers: corsHeaders },
      );
    }

    const runId = uuidv4();
    const userMessageId = `user-${Date.now()}`;

    // Save user message immediately
    const userMessage: ChatMessage = {
      id: userMessageId,
      type: "user",
      content: message,
    };
    await appendMessages(conversationId, [userMessage]);

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
      provider: "openai",
    });

    // Track start time for performance metrics
    const startTime = Date.now();

    // Helper to format response for SDK
    const formatResponseForSDK = (response: InconvoResponse): SDKResponse => {
      const base = {
        id: runId,
        conversationId,
        type: response.type,
        message: response.message,
      };
      if (response.type === "chart") {
        return {
          ...base,
          spec: response.spec,
          chart: response.chart,
        };
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
          // Save assistant response
          const assistantMessage: ChatMessage = {
            id: runId,
            type: lastResponse.type,
            content: lastResponse,
          };
          await appendMessages(conversationId, [assistantMessage]);

          // Track server-side response generation
          const posthog = getPostHogClient();
          const telemetryPayload: Parameters<typeof trackResponsePerformance>[1] = {
            success: true,
            duration_ms: Date.now() - startTime,
          };
          if (
            lastResponse.type === "text" ||
            lastResponse.type === "table" ||
            lastResponse.type === "chart"
          ) {
            telemetryPayload.response_type = lastResponse.type;
          }
          trackResponsePerformance(posthog, telemetryPayload);

          if (!conversation.title) {
            await updateConversation(conversationId, {
              title: message.slice(0, 100),
            });
          }

          return NextResponse.json(formatResponseForSDK(lastResponse), {
            headers: corsHeaders,
          });
        }

        return NextResponse.json(
          { error: "No response generated" },
          { status: 500, headers: corsHeaders },
        );
      } catch (error) {
        console.error(
          "[POST /api/v1/agents/[agentId]/conversations/[conversation_id]/response] Agent error:",
          error,
        );

        // Track server-side error
        const posthog = getPostHogClient();
        trackResponsePerformance(posthog, {
          success: false,
          duration_ms: Date.now() - startTime,
        });

        return NextResponse.json(
          {
            error:
              error instanceof Error ? error.message : "Agent execution failed",
          },
          { status: 500, headers: corsHeaders },
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
            // Save assistant response
            const assistantMessage: ChatMessage = {
              id: runId,
              type: lastResponse.type,
              content: lastResponse,
            };
            await appendMessages(conversationId, [assistantMessage]);

            // Track server-side response generation (streaming)
            const posthog = getPostHogClient();
            let responseType: "text" | "table" | "chart" | undefined;
            if (
              lastResponse.type === "text" ||
              lastResponse.type === "table" ||
              lastResponse.type === "chart"
            ) {
              responseType = lastResponse.type;
            }
            trackResponsePerformance(posthog, {
              success: true,
              duration_ms: Date.now() - startTime,
              ...(responseType && { response_type: responseType }),
            });

            sendEvent({
              type: "response.completed",
              id: runId,
              response: formatResponseForSDK(lastResponse),
            });

            if (!conversation.title) {
              await updateConversation(conversationId, {
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
            "[POST /api/v1/agents/[agentId]/conversations/[conversation_id]/response] Agent error:",
            error,
          );

          // Track server-side error (streaming)
          const posthog = getPostHogClient();
          trackResponsePerformance(posthog, {
            success: false,
            duration_ms: Date.now() - startTime,
          });

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
        ...corsHeaders,
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      },
    });
  } catch (error) {
    console.error(
      "[POST /api/v1/agents/[agentId]/conversations/[conversation_id]/response] Request error:",
      error,
    );
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Request failed",
      },
      { status: 500, headers: corsHeaders },
    );
  }
}
