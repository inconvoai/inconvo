import { relations } from "drizzle-orm/relations";
import { dim_sales_channel, fct_order, dim_traffic_channel, dim_channel, fct_order_lineitem, dim_product, fct_marketing, fct_web_analytics_traffic, fct_web_analytics_product_views } from "./schema";

export const fct_orderRelations = relations(fct_order, ({one, many}) => ({
	dim_sales_channel: one(dim_sales_channel, {
		fields: [fct_order.sales_channel_key],
		references: [dim_sales_channel.SALES_CHANNEL_KEY]
	}),
	dim_traffic_channel: one(dim_traffic_channel, {
		fields: [fct_order.traffic_channel_key],
		references: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY]
	}),
	dim_channel: one(dim_channel, {
		fields: [fct_order.CHANNEL_KEY],
		references: [dim_channel.channel_key]
	}),
	fct_order_lineitems: many(fct_order_lineitem),
}));

export const dim_sales_channelRelations = relations(dim_sales_channel, ({many}) => ({
	fct_orders: many(fct_order),
	fct_order_lineitems: many(fct_order_lineitem),
	fct_marketings: many(fct_marketing),
	fct_web_analytics_traffics: many(fct_web_analytics_traffic),
}));

export const dim_traffic_channelRelations = relations(dim_traffic_channel, ({many}) => ({
	fct_orders: many(fct_order),
	fct_order_lineitems: many(fct_order_lineitem),
	fct_marketings: many(fct_marketing),
	fct_web_analytics_traffics: many(fct_web_analytics_traffic),
}));

export const dim_channelRelations = relations(dim_channel, ({many}) => ({
	fct_orders: many(fct_order),
	fct_order_lineitems: many(fct_order_lineitem),
	fct_marketings: many(fct_marketing),
	fct_web_analytics_traffics: many(fct_web_analytics_traffic),
}));

export const fct_order_lineitemRelations = relations(fct_order_lineitem, ({one}) => ({
	dim_sales_channel: one(dim_sales_channel, {
		fields: [fct_order_lineitem.sales_channel_key],
		references: [dim_sales_channel.SALES_CHANNEL_KEY]
	}),
	dim_traffic_channel: one(dim_traffic_channel, {
		fields: [fct_order_lineitem.traffic_channel_key],
		references: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY]
	}),
	dim_channel: one(dim_channel, {
		fields: [fct_order_lineitem.CHANNEL_KEY],
		references: [dim_channel.channel_key]
	}),
	dim_product: one(dim_product, {
		fields: [fct_order_lineitem.product_key],
		references: [dim_product.product_key]
	}),
	fct_order: one(fct_order, {
		fields: [fct_order_lineitem.order_key],
		references: [fct_order._unique_key]
	}),
}));

export const dim_productRelations = relations(dim_product, ({many}) => ({
	fct_order_lineitems: many(fct_order_lineitem),
	fct_marketings: many(fct_marketing),
	fct_web_analytics_product_views: many(fct_web_analytics_product_views),
}));

export const fct_marketingRelations = relations(fct_marketing, ({one}) => ({
	dim_channel: one(dim_channel, {
		fields: [fct_marketing.CHANNEL_KEY],
		references: [dim_channel.channel_key]
	}),
	dim_traffic_channel: one(dim_traffic_channel, {
		fields: [fct_marketing.TRAFFIC_CHANNEL_KEY],
		references: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY]
	}),
	dim_sales_channel: one(dim_sales_channel, {
		fields: [fct_marketing.SALES_CHANNEL_KEY],
		references: [dim_sales_channel.SALES_CHANNEL_KEY]
	}),
	dim_product: one(dim_product, {
		fields: [fct_marketing.PRODUCT_KEY],
		references: [dim_product.product_key]
	}),
}));

export const fct_web_analytics_trafficRelations = relations(fct_web_analytics_traffic, ({one}) => ({
	dim_channel: one(dim_channel, {
		fields: [fct_web_analytics_traffic.CHANNEL_KEY],
		references: [dim_channel.channel_key]
	}),
	dim_traffic_channel: one(dim_traffic_channel, {
		fields: [fct_web_analytics_traffic.TRAFFIC_CHANNEL_KEY],
		references: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY]
	}),
	dim_sales_channel: one(dim_sales_channel, {
		fields: [fct_web_analytics_traffic.SALES_CHANNEL_KEY],
		references: [dim_sales_channel.SALES_CHANNEL_KEY]
	}),
}));

export const fct_web_analytics_product_viewsRelations = relations(fct_web_analytics_product_views, ({one}) => ({
	dim_product: one(dim_product, {
		fields: [fct_web_analytics_product_views.PRODUCT_KEY],
		references: [dim_product.product_key]
	}),
}));