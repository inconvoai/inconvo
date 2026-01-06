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
export { VegaChart, type VegaChartProps } from "./VegaChart";

// Re-export LegacyChart type from @repo/types for convenience
export type { LegacyChart } from "@repo/types";
