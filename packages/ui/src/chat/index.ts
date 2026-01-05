export {
  // Main API
  buildChartPresentation,
  getChartMeta,
  detectChartVersion,
  // Type guards
  isChartV1,
  isChartV2,
  // Constants
  CHART_COLOR_PALETTE,
  // Types
  type ChartPresentation,
  type ChartAny,
  type ChartV1,
  type ChartV2,
  type ChartVersion,
} from "./chartUtils";

export { MessageContent, type MessageContentProps } from "./MessageContent";
export { VegaChart, type VegaChartProps } from "./VegaChart";
