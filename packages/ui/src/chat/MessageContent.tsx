"use client";

import { BarChart, LineChart } from "@mantine/charts";
import { Stack, Table, Text, Box } from "@mantine/core";
import type { InconvoMessage } from "@repo/types";
import { buildChartPresentation } from "./chartUtils";

export interface MessageContentProps {
  /** The message to render */
  message: InconvoMessage;
  /** Whether the message is still streaming (shows dimmed text) */
  isStreaming?: boolean;
}

/**
 * Renders the content of an Inconvo message.
 * Handles text, chart, table, and error message types.
 *
 * This is the content only - wrap it in your own layout (Paper, Card, etc.)
 */
export function MessageContent({
  message,
  isStreaming = false,
}: MessageContentProps) {
  if (message.type === "text") {
    return (
      <Text
        style={{ whiteSpace: "pre-wrap" }}
        c={isStreaming ? "dimmed" : "black"}
      >
        {typeof message.message === "string"
          ? message.message
          : JSON.stringify(message.message)}
      </Text>
    );
  }

  if (message.type === "chart" && message.chart) {
    const result = buildChartPresentation(message.chart);

    const chartProps = {
      h: 350,
      w: "100%" as const,
      data: result.data,
      tickLine: "x" as const,
      xAxisLabel: message.chart.xLabel,
      yAxisProps: {
        label: {
          value: message.chart.yLabel,
          angle: 0,
          position: "top" as const,
          fontSize: 12,
          offset: 15,
        },
      },
      dataKey: "label",
      series: result.series,
      withLegend: true,
      p: "md" as const,
    };

    return (
      <Stack>
        <Text mb="md">{message.message}</Text>
        {message.chart.type === "bar" ? (
          <BarChart {...chartProps} />
        ) : (
          <LineChart {...chartProps} />
        )}
      </Stack>
    );
  }

  if (message.type === "table" && message.table) {
    return (
      <Stack>
        <Text mb="md">{message.message}</Text>
        <Box style={{ overflowX: "auto" }}>
          <Table striped withColumnBorders>
            <Table.Thead>
              <Table.Tr>
                {message.table.head.map((head, index) => (
                  <Table.Th key={index}>{head}</Table.Th>
                ))}
              </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
              {message.table.body.map((row: string[], rowIndex: number) => (
                <Table.Tr key={rowIndex}>
                  {row.map((cell, cellIndex) => (
                    <Table.Td key={cellIndex}>{cell}</Table.Td>
                  ))}
                </Table.Tr>
              ))}
            </Table.Tbody>
          </Table>
        </Box>
      </Stack>
    );
  }

  if (message.type === "error") {
    return (
      <Stack gap={0}>
        <Text c="red" fw={500}>
          Error:
        </Text>
        <Text c="red">{message.message}</Text>
      </Stack>
    );
  }

  return <Text c="dimmed">Unknown message type</Text>;
}
