/**
 * Versioned Chart Rendering System
 *
 * Supports multiple chart data formats without normalization.
 * Each version has its own types and presentation builder.
 */

export const CHART_COLOR_PALETTE = [
  "blue.6",
  "grape.6",
  "green.6",
  "orange.6",
  "red.6",
  "teal.6",
  "cyan.6",
  "violet.6",
] as const;

// ============================================================================
// Chart Version Types
// ============================================================================

/**
 * V1 (Legacy): Simple array of label/value pairs
 * Used in early versions, single dataset only
 */
export interface ChartV1 {
  type: "bar" | "line";
  xLabel?: string;
  yLabel?: string;
  data: Array<{ label: string; value: number }>;
}

/**
 * V2 (Current): Multiple datasets with shared labels
 * Supports multiple series on the same chart
 */
export interface ChartV2 {
  type: "bar" | "line";
  xLabel?: string;
  yLabel?: string;
  data: {
    labels: string[];
    datasets: Array<{ name: string; values: number[] }>;
  };
}

// Future: V3 could support time series, stacked charts, etc.
// export interface ChartV3 { ... }

export type ChartAny = ChartV1 | ChartV2;
export type ChartVersion = 1 | 2;

// ============================================================================
// Version Detection
// ============================================================================

export function detectChartVersion(chart: ChartAny): ChartVersion {
  if (Array.isArray(chart.data)) {
    return 1;
  }
  if (
    chart.data &&
    typeof chart.data === "object" &&
    "datasets" in chart.data
  ) {
    return 2;
  }
  // Default to latest if unknown structure
  return 2;
}

export function isChartV1(chart: ChartAny): chart is ChartV1 {
  return detectChartVersion(chart) === 1;
}

export function isChartV2(chart: ChartAny): chart is ChartV2 {
  return detectChartVersion(chart) === 2;
}

// ============================================================================
// Chart Presentation (consumed by Mantine Charts)
// ============================================================================

export interface ChartPresentation {
  data: Array<Record<string, string | number>>;
  series: Array<{ name: string; color: string }>;
}

// ============================================================================
// Version-Specific Presentation Builders
// ============================================================================

function buildPresentationV1(chart: ChartV1): ChartPresentation {
  const seriesName = chart.yLabel || "value";

  const data = chart.data.map((point) => ({
    label: point.label,
    [seriesName]: point.value,
  }));

  const series = [
    {
      name: seriesName,
      color: CHART_COLOR_PALETTE[0]!,
    },
  ];

  return { data, series };
}

function buildPresentationV2(chart: ChartV2): ChartPresentation {
  const { labels, datasets } = chart.data;

  const data = labels.map((label, labelIndex) => {
    const row: Record<string, string | number> = { label };

    datasets.forEach(({ name, values }) => {
      const value = values[labelIndex];
      if (value === undefined) {
        throw new Error(
          `Dataset "${name}" is missing a value for label "${label}".`,
        );
      }
      row[name] = value;
    });

    return row;
  });

  if (datasets.length > CHART_COLOR_PALETTE.length) {
    console.warn(
      `There are more datasets (${datasets.length}) than available colors (${CHART_COLOR_PALETTE.length}). Colors will repeat.`,
    );
  }

  const series = datasets.map(({ name }, index) => ({
    name,
    color: CHART_COLOR_PALETTE[index % CHART_COLOR_PALETTE.length]!,
  }));

  return { data, series };
}

// ============================================================================
// Version-Specific Renderers Registry
// ============================================================================

const presentationBuilders: Record<
  ChartVersion,
  (chart: ChartAny) => ChartPresentation
> = {
  1: (chart) => buildPresentationV1(chart as ChartV1),
  2: (chart) => buildPresentationV2(chart as ChartV2),
};

// ============================================================================
// Main Entry Point
// ============================================================================

/**
 * Build chart presentation data for Mantine Charts.
 * Automatically detects chart version and uses appropriate builder.
 */
export function buildChartPresentation(chart: ChartAny): ChartPresentation {
  const version = detectChartVersion(chart);
  const builder = presentationBuilders[version];
  return builder(chart);
}

/**
 * Get chart metadata (type, labels) regardless of version
 */
export function getChartMeta(chart: ChartAny): {
  type: "bar" | "line";
  xLabel?: string;
  yLabel?: string;
} {
  return {
    type: chart.type,
    xLabel: chart.xLabel,
    yLabel: chart.yLabel,
  };
}
