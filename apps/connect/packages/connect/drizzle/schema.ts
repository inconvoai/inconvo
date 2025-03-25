import { pgTable, foreignKey, text, integer, timestamp, doublePrecision, numeric, boolean } from "drizzle-orm/pg-core"
import { sql } from "drizzle-orm"



export const fct_order = pgTable("fct_order", {
	_unique_key: text().primaryKey().notNull(),
	order_status_key: text(),
	order_financial_status_key: integer(),
	order_fulfillment_status_key: integer(),
	customer_key: text(),
	store_key: text(),
	data_source_key: text(),
	sales_channel_key: integer(),
	traffic_channel_key: integer(),
	platform_key: integer(),
	CHANNEL_KEY: integer(),
	utm_key: text(),
	device_key: integer(),
	billing_territory_key: integer(),
	shipping_territory_key: integer(),
	shipping_province_key: integer(),
	web_analytics_territory_key: integer(),
	customer_order_number: integer(),
	promo_code_key: text(),
	order_tags_key: text(),
	time_between_orders: integer(),
	ORDER_TIMESTAMP: timestamp({ mode: 'string' }),
	ORDER_PRODUCT_QUANTITY: integer(),
	ORDER_PRODUCT_ORIGINAL_PRICE: integer(),
	ORDER_PRODUCT_GROSS_DISCOUNT: integer(),
	ORDER_PRODUCT_GROSS_REVENUE: integer(),
	ORDER_PRODUCT_GROSS_REFUND: integer(),
	ORDER_PRODUCT_TAX: integer(),
	ORDER_PRODUCT_NET_REFUND: integer(),
	ORDER_PRODUCT_COGS: integer(),
	ORDER_SHIPPING_ORIGINAL_PRICE: integer(),
	ORDER_SHIPPING_GROSS_DISCOUNT: integer(),
	ORDER_SHIPPING_GROSS_REVENUE: integer(),
	ORDER_SHIPPING_GROSS_REFUND: integer(),
	ORDER_SHIPPING_TAX: integer(),
	ORDER_SHIPPING_NET_REFUND: integer(),
	ORDER_SHIPPING_COGS: integer(),
}, (table) => [
	foreignKey({
			columns: [table.sales_channel_key],
			foreignColumns: [dim_sales_channel.SALES_CHANNEL_KEY],
			name: "fk_fct_order_sales_channel_key"
		}),
	foreignKey({
			columns: [table.traffic_channel_key],
			foreignColumns: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY],
			name: "fk_fct_order_traffic_channel_key"
		}),
	foreignKey({
			columns: [table.CHANNEL_KEY],
			foreignColumns: [dim_channel.channel_key],
			name: "fk_fct_order_channel_key"
		}),
]);

export const dim_order = pgTable("dim_order", {
	order_key: text().primaryKey().notNull(),
});

export const fct_order_lineitem = pgTable("fct_order_lineitem", {
	_unique_key: text().primaryKey().notNull(),
	order_key: text(),
	order_status_key: text(),
	order_financial_status_key: integer(),
	order_fulfillment_status_key: integer(),
	product_key: text(),
	warehouse_key: text(),
	customer_key: text(),
	store_key: text(),
	data_source_key: text(),
	billing_territory_key: integer(),
	shipping_territory_key: integer(),
	shipping_province_key: integer(),
	sales_channel_key: integer(),
	platform_key: integer(),
	CHANNEL_KEY: integer(),
	traffic_channel_key: integer(),
	utm_key: text(),
	device_key: integer(),
	web_analytics_territory_key: integer(),
	customer_order_number: integer(),
	promo_code_key: text(),
	order_tags_key: text(),
	ORDER_TIMESTAMP: timestamp({ mode: 'string' }),
	num_orders: doublePrecision(),
	order_is_zero_value_order_flag: integer(),
	ORDER_LINEITEM_PRODUCT_QUANTITY: integer(),
	ORDER_LINEITEM_PRODUCT_ORIGINAL_PRICE: integer(),
	ORDER_LINEITEM_PRODUCT_GROSS_DISCOUNT: integer(),
	ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: integer(),
	ORDER_LINEITEM_PRODUCT_TAX: integer(),
	ORDER_LINEITEM_PRODUCT_QUANTITY_REFUNDED: integer(),
	ORDER_LINEITEM_PRODUCT_GROSS_REFUND: integer(),
	ORDER_LINEITEM_PRODUCT_NET_REFUND: integer(),
	ORDER_LINEITEM_PRODUCT_COGS: integer(),
	ORDER_LINEITEM_SHIPPING_QUANTITY: integer(),
	ORDER_LINEITEM_SHIPPING_ORIGINAL_PRICE: integer(),
	ORDER_LINEITEM_SHIPPING_GROSS_DISCOUNT: integer(),
	ORDER_LINEITEM_SHIPPING_GROSS_REVENUE: integer(),
	ORDER_LINEITEM_SHIPPING_GROSS_REFUND: integer(),
	ORDER_LINEITEM_SHIPPING_TAX: integer(),
	ORDER_LINEITEM_SHIPPING_NET_REFUND: integer(),
	ORDER_LINEITEM_SHIPPING_COGS: integer(),
	order_lineitem_per_order_costs: doublePrecision(),
	order_lineitem_merchant_item_fees: doublePrecision(),
	order_lineitem_merchant_item_fees_refunded: doublePrecision(),
	order_lineitem_is_zero_value_flag: integer(),
}, (table) => [
	foreignKey({
			columns: [table.sales_channel_key],
			foreignColumns: [dim_sales_channel.SALES_CHANNEL_KEY],
			name: "fk_fct_order_lineitem_sales_channel_key"
		}),
	foreignKey({
			columns: [table.traffic_channel_key],
			foreignColumns: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY],
			name: "fk_fct_order_lineitem_traffic_channel_key"
		}),
	foreignKey({
			columns: [table.CHANNEL_KEY],
			foreignColumns: [dim_channel.channel_key],
			name: "fk_fct_order_lineitem_channel_key"
		}),
	foreignKey({
			columns: [table.product_key],
			foreignColumns: [dim_product.product_key],
			name: "fk_fct_order_lineitem_product_key"
		}),
	foreignKey({
			columns: [table.order_key],
			foreignColumns: [fct_order._unique_key],
			name: "fk_fct_order_lineitem_order_key"
		}),
]);

export const dim_traffic_channel = pgTable("dim_traffic_channel", {
	TRAFFIC_CHANNEL_KEY: integer().primaryKey().notNull(),
	CHANNEL_NAME: text(),
	CHANNEL_GROUPING: text(),
	CHANNEL_TYPE: text(),
});

export const dim_channel = pgTable("dim_channel", {
	channel_key: integer().primaryKey().notNull(),
	PLATFORM_NAME: text(),
	CHANNEL_NAME: text(),
	CHANNEL_GROUPING: text(),
	channel_type: text(),
});

export const fct_marketing = pgTable("fct_marketing", {
	_UNIQUE_KEY: text().primaryKey().notNull(),
	UTM_KEY: text(),
	WEB_UTM_KEY: text(),
	CHANNEL_KEY: integer(),
	TRAFFIC_CHANNEL_KEY: integer(),
	SALES_CHANNEL_KEY: integer(),
	DEVICE_KEY: numeric(),
	MARKETING_TERRITORY_KEY: numeric(),
	STORE_KEY: text(),
	DATA_SOURCE_KEY: text(),
	PRODUCT_KEY: text(),
	PLATFORM_KEY: numeric(),
	MARKETING_ACCOUNT_KEY: text(),
	marketing_date: timestamp({ mode: 'string' }),
	marketing_impressions: integer(),
	marketing_clicks: integer(),
	marketing_max_search_impressions: integer(),
	marketing_impressions_max_search_impressions: integer(),
	marketing_spend: integer(),
	marketing_platform_conversions: integer(),
	marketing_platform_revenue: integer(),
}, (table) => [
	foreignKey({
			columns: [table.CHANNEL_KEY],
			foreignColumns: [dim_channel.channel_key],
			name: "fk_fct_marketing_channel_key"
		}),
	foreignKey({
			columns: [table.TRAFFIC_CHANNEL_KEY],
			foreignColumns: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY],
			name: "fk_fct_marketing_traffic_channel_key"
		}),
	foreignKey({
			columns: [table.SALES_CHANNEL_KEY],
			foreignColumns: [dim_sales_channel.SALES_CHANNEL_KEY],
			name: "fk_fct_marketing_sales_channel_key"
		}),
	foreignKey({
			columns: [table.PRODUCT_KEY],
			foreignColumns: [dim_product.product_key],
			name: "fk_fct_marketing_product_key"
		}),
]);

export const dim_sales_channel = pgTable("dim_sales_channel", {
	SALES_CHANNEL_KEY: integer().primaryKey().notNull(),
	SALES_CHANNEL: text(),
	IS_ECOMMERCE: boolean(),
});

export const dim_product = pgTable("dim_product", {
	product_key: text().primaryKey().notNull(),
	product_sku: text(),
	PRODUCT_BRAND: text(),
	product_tags: text(),
	product_collection_names: text(),
	PRODUCT_NAME: text(),
	PRODUCT_CATEGORY: text(),
	product_category_2: text(),
	product_category_3: text(),
	product_color: text(),
	product_size: text(),
	PRODUCT_KEY_IMAGE: text(),
	PRODUCT_SKU_IMAGE: text(),
	PRODUCT_NAME_IMAGE: text(),
	IMAGE_SKU: text(),
	IMAGE_NAME: text(),
	PRODUCT_IMAGE: text(),
	product_state: text(),
});

export const fct_web_analytics_traffic = pgTable("fct_web_analytics_traffic", {
	_UNIQUE_KEY: text().primaryKey().notNull(),
	UTM_KEY: text(),
	CHANNEL_KEY: integer(),
	TRAFFIC_CHANNEL_KEY: integer(),
	SALES_CHANNEL_KEY: integer(),
	PLATFORM_KEY: numeric(),
	DEVICE_KEY: numeric(),
	WEB_ANALYTICS_TERRITORY_KEY: numeric(),
	STORE_KEY: text(),
	DATA_SOURCE_KEY: text(),
	web_analytics_traffic_date: timestamp({ mode: 'string' }),
	web_analytics_traffic_sessions: integer(),
	web_analytics_traffic_newusers: integer(),
}, (table) => [
	foreignKey({
			columns: [table.CHANNEL_KEY],
			foreignColumns: [dim_channel.channel_key],
			name: "fk_fct_web_analytics_traffic_channel_key"
		}),
	foreignKey({
			columns: [table.TRAFFIC_CHANNEL_KEY],
			foreignColumns: [dim_traffic_channel.TRAFFIC_CHANNEL_KEY],
			name: "fk_fct_web_analytics_traffic_traffic_channel_key"
		}),
	foreignKey({
			columns: [table.SALES_CHANNEL_KEY],
			foreignColumns: [dim_sales_channel.SALES_CHANNEL_KEY],
			name: "fk_fct_web_analytics_traffic_sales_channel_key"
		}),
]);

export const fct_web_analytics_product_views = pgTable("fct_web_analytics_product_views", {
	_UNIQUE_KEY: text().primaryKey().notNull(),
	STORE_KEY: text(),
	DATA_SOURCE_KEY: text(),
	PRODUCT_KEY: text(),
	WEB_ANALYTICS_TERRITORY_KEY: numeric(),
	WEB_ANALYTICS_PRODUCT_VIEW_DATE: timestamp({ mode: 'string' }),
	WEB_ANALYTICS_PRODUCT_PAGE_ENTRANCES: integer(),
	WEB_ANALYTICS_PRODUCT_PAGE_EXITS: integer(),
	WEB_ANALYTICS_PRODUCT_PAGE_VIEWS: integer(),
	WEB_ANALYTICS_PRODUCT_PAGE_AVG_TIME_ON_PAGE: numeric(),
	WEB_ANALYTICS_PRODUCT_LED_CONVERSIONS: numeric(),
	WEB_ANALYTICS_PRODUCT_LED_CONVERSION_REVENUE: numeric(),
	WEB_ANALYTICS_CROSS_SELL_CONVERSIONS: doublePrecision(),
	WEB_ANALYTICS_CROSS_SELL_CONVERSIONS_REVENUE: doublePrecision(),
}, (table) => [
	foreignKey({
			columns: [table.PRODUCT_KEY],
			foreignColumns: [dim_product.product_key],
			name: "fk_fct_web_analytics_product_views_product_key"
		}),
]);
