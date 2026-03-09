import { describe, expect, it } from "vitest";
import {
  buildJoinDescriptorDescription,
  buildJoinPathMismatchMessage,
} from "./joinGuidance";

describe("joinGuidance", () => {
  it("explains omitted name using the displayed name", () => {
    const description = buildJoinDescriptorDescription("Provide joins.", [
      {
        name: "users.orders",
        table: "orders",
        path: [
          {
            source: ["users.id"],
            target: ["orders.user_id"],
          },
        ],
      },
    ]);

    expect(description).toContain(
      "Omit name to use the name shown before parentheses.",
    );
  });

  it("shows the only available correction when there is a single join option", () => {
    const message = buildJoinPathMismatchMessage(
      {
        table: "wrong_table",
        name: "wrong_name",
        path: [
          {
            source: ["users.email"],
            target: ["orders.invalid"],
          },
        ],
      },
      [
        {
          name: "users.orders",
          table: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
          ],
        },
      ],
    );

    expect(message).toBe(
      "Join path does not match the available relation path for users.orders (orders). Use path=[users.id] -> [orders.user_id] exactly.",
    );
  });

  it("falls back to the generic message when no likely option is found", () => {
    const message = buildJoinPathMismatchMessage(
      {
        table: "unrelated",
        name: "unknown",
        path: [
          {
            source: ["users.email"],
            target: ["unknown.invalid"],
          },
        ],
      },
      [
        {
          name: "users.orders",
          table: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
          ],
        },
        {
          name: "users.sessions",
          table: "sessions",
          path: [
            {
              source: ["users.id"],
              target: ["sessions.user_id"],
            },
          ],
        },
      ],
    );

    expect(message).toBe(
      "Join path does not match any available relation path.",
    );
  });
});
