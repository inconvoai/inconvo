import { relations } from "drizzle-orm/relations";
import { products, orders, people, reviews } from "./schema";

export const ordersRelations = relations(orders, ({one}) => ({
	product: one(products, {
		fields: [orders.productId],
		references: [products.id]
	}),
	person: one(people, {
		fields: [orders.userId],
		references: [people.id]
	}),
}));

export const productsRelations = relations(products, ({many}) => ({
	orders: many(orders),
	reviews: many(reviews),
}));

export const peopleRelations = relations(people, ({many}) => ({
	orders: many(orders),
}));

export const reviewsRelations = relations(reviews, ({one}) => ({
	product: one(products, {
		fields: [reviews.productId],
		references: [products.id]
	}),
}));