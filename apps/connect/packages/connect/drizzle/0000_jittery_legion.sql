-- Current sql file was generated after introspecting the database
-- If you want to run this migration please uncomment this code before executing migrations
/*
CREATE TABLE "products" (
	"id" integer PRIMARY KEY NOT NULL,
	"ean" char(13),
	"title" varchar(255),
	"category" varchar(255),
	"vendor" varchar(255),
	"price" double precision,
	"rating" double precision,
	"created_at" timestamp
);
--> statement-breakpoint
CREATE TABLE "orders" (
	"id" integer PRIMARY KEY NOT NULL,
	"user_id" integer,
	"product_id" integer,
	"subtotal" double precision,
	"tax" double precision,
	"total" double precision,
	"discount" double precision,
	"created_at" timestamp,
	"quantity" integer
);
--> statement-breakpoint
CREATE TABLE "people" (
	"id" integer PRIMARY KEY NOT NULL,
	"address" varchar(255),
	"email" varchar(255),
	"password" varchar(255),
	"name" varchar(255),
	"city" varchar(255),
	"longitude" double precision,
	"state" char(2),
	"source" varchar(255),
	"birth_date" date,
	"zip" char(5),
	"latitude" double precision,
	"created_at" timestamp
);
--> statement-breakpoint
CREATE TABLE "reviews" (
	"id" integer PRIMARY KEY NOT NULL,
	"product_id" integer,
	"reviewer" varchar(255),
	"rating" smallint,
	"body" text,
	"created_at" timestamp
);
--> statement-breakpoint
ALTER TABLE "orders" ADD CONSTRAINT "fk_orders_product_id_products" FOREIGN KEY ("product_id") REFERENCES "public"."products"("id") ON DELETE no action ON UPDATE no action;--> statement-breakpoint
ALTER TABLE "orders" ADD CONSTRAINT "fk_orders_user_id_people" FOREIGN KEY ("user_id") REFERENCES "public"."people"("id") ON DELETE no action ON UPDATE no action;--> statement-breakpoint
ALTER TABLE "reviews" ADD CONSTRAINT "fk_reviews_product_id_products" FOREIGN KEY ("product_id") REFERENCES "public"."products"("id") ON DELETE no action ON UPDATE no action;
*/