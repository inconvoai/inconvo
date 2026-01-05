"use client";

import { Stack, Table, Text, Box } from "@mantine/core";
import { BarChart, LineChart } from "@mantine/charts";
import type { InconvoMessage } from "@repo/types";
import {
  buildChartPresentation,
  getChartMeta,
  type ChartAny,
} from "./chartUtils";
import { VegaChart } from "./VegaChart";

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

  if (message.type === "chart") {
    // Preferred path: Vega-Lite spec
    if (message.spec) {
      return (
        <Stack>
          <Text mb="md">{message.message}</Text>
          <Box w="100%" style={{ overflowX: "auto" }}>
            <VegaChart spec={message.spec} />
          </Box>
        </Stack>
      );
    }

    // Legacy path: structured bar/line payloads for historical logs
    const legacyChart = (message as InconvoMessage & {
      chart?: ChartAny;
    }).chart;

    if (legacyChart) {
      const { data, series } = buildChartPresentation(legacyChart);
      const { type, xLabel, yLabel } = getChartMeta(legacyChart);
      const ChartComponent = type === "bar" ? BarChart : LineChart;

      return (
        <Stack>
          <Text mb="md">{message.message}</Text>
          <Box style={{ overflowX: "auto" }}>
            <ChartComponent
              data={data}
              dataKey="label"
              h={300}
              withLegend
              xAxisLabel={xLabel}
              yAxisLabel={yLabel}
              tooltipAnimationDuration={150}
              series={series.map((s) => ({ name: s.name, color: s.color }))}
            />
          </Box>
        </Stack>
      );
    }
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
