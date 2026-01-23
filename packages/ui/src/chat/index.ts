export {
  // Main API
  buildChartPresentation,
  getChartMeta,
  // Constants
  CHART_COLOR_PALETTE,
  // Types
  type ChartPresentation,
} from "./chartUtils";

export { MessageContent, type MessageContentProps } from "./MessageContent";
// VegaChart is NOT exported here to avoid module-level evaluation of vega libs
// which causes RSC serialization errors. Import directly from "./VegaChart" with lazy loading.
export type { VegaChartProps } from "./VegaChart";

// Re-export LegacyChart type from @repo/types for convenience
export type { LegacyChart } from "@repo/types";
