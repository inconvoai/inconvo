"use client";

import { Box, Text } from "@mantine/core";
import type { VegaLiteSpec } from "@repo/types";
import { memo, useEffect, useMemo, useState } from "react";
import type { VisualizationSpec } from "react-vega";
import { VegaLite } from "react-vega";

export interface VegaChartProps {
  /** Vega-Lite v5 specification */
  spec: VegaLiteSpec;
  /** Chart width - defaults to "container" for responsive sizing */
  width?: number | "container";
  /** Chart height - defaults to DEFAULT_CHART_HEIGHT (350) */
  height?: number;
}

const DEFAULT_CHART_WIDTH = 500;
const DEFAULT_CHART_HEIGHT = 350;

/**
 * Renders a Vega-Lite v5 visualization.
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
          $schema: "https://vega.github.io/schema/vega-lite/v5.json",
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

    const handleError = (err: Error) => {
      console.error("Vega-Lite render error:", err);
      setError(err.message);
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
        <VegaLite spec={fullSpec} actions={false} onError={handleError} />
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
