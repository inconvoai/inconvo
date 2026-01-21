"use client";

import { Box, Text } from "@mantine/core";
import type { VegaLiteSpec } from "@repo/types";
import { memo, useEffect, useMemo, useState } from "react";
import { VegaEmbed } from "react-vega";
import type { VisualizationSpec } from "vega-embed";

export interface VegaChartProps {
  /** Vega-Lite specification */
  spec: VegaLiteSpec;
  /** Chart width - defaults to "container" for responsive sizing */
  width?: number | "container";
  /** Chart height - defaults to DEFAULT_CHART_HEIGHT (350) */
  height?: number;
}

const DEFAULT_CHART_WIDTH = 500;
const DEFAULT_CHART_HEIGHT = 350;

/**
 * Renders a Vega-Lite v6 visualization.
 * Handles responsive sizing and error states.
 */
export const VegaChart = memo(
  function VegaChart({
    spec,
    width = DEFAULT_CHART_WIDTH,
    height = DEFAULT_CHART_HEIGHT,
  }: VegaChartProps) {
    const [error, setError] = useState<string | null>(null);

    // Merge default sizing with spec, memoized to prevent unnecessary re-renders
    // Cast to VisualizationSpec since our Zod schema is looser than the library types
    const fullSpec = useMemo(
      () =>
        ({
          $schema: "https://vega.github.io/schema/vega-lite/v6.json",
          ...spec,
          width: spec.width ?? width,
          height: spec.height ?? height,
          autosize:
            spec.autosize ??
            (width === "container"
              ? { type: "fit", contains: "padding" }
              : undefined),
        }) as VisualizationSpec,
      [spec, width, height],
    );

    useEffect(() => {
      setError(null);
    }, [spec]);

    const handleError = (err: unknown) => {
      console.error("Vega-Lite render error:", err);
      setError(err instanceof Error ? err.message : String(err));
    };

    if (error) {
      return (
        <Box
          p="md"
          style={{
            border: "1px solid var(--mantine-color-red-3)",
            borderRadius: "var(--mantine-radius-sm)",
            backgroundColor: "var(--mantine-color-red-0)",
          }}
        >
          <Text c="red" fw={500} size="sm">
            Chart Error
          </Text>
          <Text c="red" size="sm">
            {error}
          </Text>
        </Box>
      );
    }

    return (
      <Box style={{ width: "100%" }}>
        <VegaEmbed
          spec={fullSpec}
          options={{ actions: false }}
          onError={handleError}
        />
      </Box>
    );
  },
  (prevProps, nextProps) => {
    // Deep compare specs to prevent unnecessary re-renders
    return (
      prevProps.width === nextProps.width &&
      prevProps.height === nextProps.height &&
      JSON.stringify(prevProps.spec) === JSON.stringify(nextProps.spec)
    );
  },
);
