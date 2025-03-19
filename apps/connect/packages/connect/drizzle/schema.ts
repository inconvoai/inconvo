import { pgTable, integer, char, varchar, doublePrecision, timestamp, foreignKey, date, smallint, text } from "drizzle-orm/pg-core"
import { sql } from "drizzle-orm"



export const products = pgTable("products", {
	id: integer().primaryKey().notNull(),
	ean: char({ length: 13 }),
	title: varchar({ length: 255 }),
	category: varchar({ length: 255 }),
	vendor: varchar({ length: 255 }),
	price: doublePrecision(),
	rating: doublePrecision(),
	createdAt: timestamp("created_at", { mode: 'string' }),
});

export const orders = pgTable("orders", {
	id: integer().primaryKey().notNull(),
	userId: integer("user_id"),
	productId: integer("product_id"),
	subtotal: doublePrecision(),
	tax: doublePrecision(),
	total: doublePrecision(),
	discount: doublePrecision(),
	createdAt: timestamp("created_at", { mode: 'string' }),
	quantity: integer(),
}, (table) => [
	foreignKey({
			columns: [table.productId],
			foreignColumns: [products.id],
			name: "fk_orders_product_id_products"
		}),
	foreignKey({
			columns: [table.userId],
			foreignColumns: [people.id],
			name: "fk_orders_user_id_people"
		}),
]);

export const people = pgTable("people", {
	id: integer().primaryKey().notNull(),
	address: varchar({ length: 255 }),
	email: varchar({ length: 255 }),
	password: varchar({ length: 255 }),
	name: varchar({ length: 255 }),
	city: varchar({ length: 255 }),
	longitude: doublePrecision(),
	state: char({ length: 2 }),
	source: varchar({ length: 255 }),
	birthDate: date("birth_date"),
	zip: char({ length: 5 }),
	latitude: doublePrecision(),
	createdAt: timestamp("created_at", { mode: 'string' }),
});

export const reviews = pgTable("reviews", {
	id: integer().primaryKey().notNull(),
	productId: integer("product_id"),
	reviewer: varchar({ length: 255 }),
	rating: smallint(),
	body: text(),
	createdAt: timestamp("created_at", { mode: 'string' }),
}, (table) => [
	foreignKey({
			columns: [table.productId],
			foreignColumns: [products.id],
			name: "fk_reviews_product_id_products"
		}),
]);
