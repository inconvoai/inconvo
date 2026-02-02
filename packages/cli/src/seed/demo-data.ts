/**
 * Populates the demo database with synthetic data using @faker-js/faker.
 *
 * Per organisation (Apple = 1, Tesla = 2, Logitech = 3):
 *   - 10,000 users
 *   - ~100,000 orders (with high variance distribution)
 *   - 5,000 reviews
 */
import { faker } from "@faker-js/faker";
import pg from "pg";

const { Client } = pg;

// US metro locations for realistic user data
const LOCALES = [
  { city: "New York", state: "NY", lat: 40.7128, lon: -74.006 },
  { city: "Los Angeles", state: "CA", lat: 34.0522, lon: -118.2437 },
  { city: "Chicago", state: "IL", lat: 41.8781, lon: -87.6298 },
  { city: "San Francisco", state: "CA", lat: 37.7749, lon: -122.4194 },
  { city: "Houston", state: "TX", lat: 29.7604, lon: -95.3698 },
  { city: "Miami", state: "FL", lat: 25.7617, lon: -80.1918 },
  { city: "Dallas", state: "TX", lat: 32.7767, lon: -96.797 },
  { city: "Boston", state: "MA", lat: 42.3601, lon: -71.0589 },
  { city: "Seattle", state: "WA", lat: 47.6062, lon: -122.3321 },
  { city: "Atlanta", state: "GA", lat: 33.749, lon: -84.388 },
  { city: "Phoenix", state: "AZ", lat: 33.4484, lon: -112.074 },
  { city: "Philadelphia", state: "PA", lat: 39.9526, lon: -75.1652 },
  { city: "Denver", state: "CO", lat: 39.7392, lon: -104.9903 },
  { city: "Las Vegas", state: "NV", lat: 36.1699, lon: -115.1398 },
  { city: "San Diego", state: "CA", lat: 32.7157, lon: -117.1611 },
  { city: "Austin", state: "TX", lat: 30.2672, lon: -97.7431 },
  { city: "Orlando", state: "FL", lat: 28.5383, lon: -81.3792 },
  { city: "Portland", state: "OR", lat: 45.5051, lon: -122.675 },
  { city: "Charlotte", state: "NC", lat: 35.2271, lon: -80.8431 },
  { city: "San Jose", state: "CA", lat: 37.3382, lon: -121.8863 },
];

const jitter = (val: number, range = 0.05): number =>
  val + faker.number.float({ min: -range, max: range, multipleOf: 0.0001 });

const START = new Date("2023-01-01");
const END = new Date();
const randDate = (): Date => faker.date.between({ from: START, to: END });

// Power law distribution helper - creates more realistic variance
const powerLawRandom = (min: number, max: number, exponent = 2): number => {
  const r = Math.pow(Math.random(), exponent);
  return Math.floor(min + r * (max - min + 1));
};

// Pareto distribution - creates high variance with a long tail
const paretoRandom = (min: number, max: number, alpha = 1.5): number => {
  const r = 1 / Math.pow(Math.random(), 1 / alpha);
  return Math.min(max, Math.floor(min * r));
};

// Generate random discount amount (0-5% of subtotal)
const calculateDiscount = (subtotal: number): number => {
  const percentage = Math.random() * 0.05;
  return Number((subtotal * percentage).toFixed(2));
};

interface OrgData {
  id: number;
  productIds: number[];
}

interface InsertedUser {
  id: number;
  name: string;
  email: string;
  password: string;
  address: string;
  city: string;
  zip: string;
  latitude: number;
  longitude: number;
  birth_date: Date;
  created_at: Date;
  last_order_at: Date | null;
}

interface OrderData {
  user_id: number;
  product_id: number;
  quantity: number;
  subtotal: number;
  tax: number;
  discount: number;
  created_at: Date;
}

interface ReviewData {
  user_id: number;
  product_id: number;
  rating: number;
  body: string;
  created_at: Date;
}

async function batchInsert<T>(
  client: pg.Client,
  base: string,
  rows: T[],
  map: (row: T) => unknown[],
  size = 1_000
): Promise<void> {
  if (!rows.length) return;
  const firstRow = rows[0];
  if (!firstRow) return;
  const cols = map(firstRow).length;
  for (let i = 0; i < rows.length; i += size) {
    const slice = rows.slice(i, i + size);
    const placeholders = slice
      .map(
        (_, r) =>
          "(" +
          Array.from({ length: cols }, (_, c) => `$${r * cols + c + 1}`).join(
            ","
          ) +
          ")"
      )
      .join(",");
    await client.query(base + " VALUES " + placeholders, slice.flatMap(map));
  }
}

async function updateLastOrder(
  client: pg.Client,
  orgId: number,
  ids: number[],
  dates: Date[]
): Promise<void> {
  if (ids.length === 0) return;
  await client.query(
    `UPDATE public.users AS c
       SET last_order_at = v.last_order_at
       FROM (SELECT unnest($1::int[]) AS id, unnest($2::timestamptz[]) AS last_order_at) AS v
       WHERE c.organisation_id = $3 AND c.id = v.id;`,
    [ids, dates, orgId]
  );
}

export async function seedDemoData(connectionString: string): Promise<void> {
  // Set a consistent seed for reproducible data
  const SEED = 42;
  faker.seed(SEED);

  const client = new Client({ connectionString });
  await client.connect();

  try {
    // Check if data already exists
    const { rows: existingUsers } = await client.query(
      "SELECT COUNT(*) as count FROM public.users"
    );
    if (parseInt(existingUsers[0].count) > 0) {
      console.log("  Demo data already exists, skipping seed");
      return;
    }

    console.log("  Seeding demo data...");

    // Load per-tenant product IDs
    const ORGS: OrgData[] = [];
    for (const orgId of [1, 2, 3]) {
      const { rows } = await client.query(
        "SELECT id FROM public.products WHERE organisation_id = $1 ORDER BY id",
        [orgId]
      );
      if (!rows.length) throw new Error(`No products found for org ${orgId}`);
      ORGS.push({ id: orgId, productIds: rows.map((r) => r.id as number) });
    }

    for (const org of ORGS) {
      // Generate users
      const usersToInsert = Array.from({ length: 10_000 }, () => {
        const loc = faker.helpers.arrayElement(LOCALES);
        const joined = randDate();
        return {
          name: faker.person.fullName(),
          email: faker.internet.email({ provider: "example.com" }),
          password: faker.internet.password(),
          address: faker.location.streetAddress() + ", " + loc.city,
          city: loc.city,
          zip: faker.location.zipCode("#####"),
          latitude: jitter(loc.lat),
          longitude: jitter(loc.lon),
          birth_date: faker.date.birthdate({ min: 18, max: 65, mode: "age" }),
          created_at: joined,
          last_order_at: null as Date | null,
        };
      });

      // Insert users and collect the generated IDs
      const userInsertQuery = `
        INSERT INTO public.users
          (organisation_id,name,email,password,address,city,zip,latitude,longitude,birth_date,created_at,last_order_at)
        VALUES
          ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)
        RETURNING id, created_at`;

      const insertedUsers: InsertedUser[] = [];
      for (let i = 0; i < usersToInsert.length; i += 1000) {
        const batch = usersToInsert.slice(i, i + 1000);
        const batchPromises = batch.map((c) =>
          client.query(userInsertQuery, [
            org.id,
            c.name,
            c.email,
            c.password,
            c.address,
            c.city,
            c.zip,
            c.latitude,
            c.longitude,
            c.birth_date,
            c.created_at,
            c.last_order_at,
          ])
        );
        const results = await Promise.all(batchPromises);
        for (let j = 0; j < results.length; j++) {
          const result = results[j];
          const batchItem = batch[j];
          if (result && batchItem && result.rows[0]) {
            insertedUsers.push({
              ...batchItem,
              id: result.rows[0].id as number,
            });
          }
        }
      }

      // Create user ID map for O(1) lookups
      const userMap = new Map<number, InsertedUser>();
      insertedUsers.forEach((user) => userMap.set(user.id, user));

      // Orders with high variance in distribution
      const lastOrderMap = new Map<number, Date>();
      const productOrderCounts = new Map<number, number>();
      org.productIds.forEach((id) => productOrderCounts.set(id, 0));

      // 3% of users will have no orders
      const noOrderPercentage = 0.03;
      const hasOrdersMap = new Map<number, boolean>();

      for (const user of insertedUsers) {
        hasOrdersMap.set(user.id, Math.random() >= noOrderPercentage);
      }

      const customerOrderFrequency = new Map<number, number>();
      for (const user of insertedUsers) {
        const hasOrders = hasOrdersMap.get(user.id);
        customerOrderFrequency.set(
          user.id,
          hasOrders ? powerLawRandom(1, 50, 4) : 0
        );
      }

      // Generate baseline orders
      const baseline: OrderData[] = [];
      for (const c of insertedUsers) {
        if (!hasOrdersMap.get(c.id)) continue;

        const qty = paretoRandom(1, 20, 1.8);
        const price = Number(
          faker.commerce.price({ min: 20, max: 1_500, dec: 2 })
        );
        const tax = Number((price * 0.1).toFixed(2));
        const discount = calculateDiscount(price);
        const when = faker.date.between({ from: c.created_at, to: END });
        lastOrderMap.set(c.id, when);

        const productId = faker.helpers.arrayElement(org.productIds);
        productOrderCounts.set(
          productId,
          (productOrderCounts.get(productId) ?? 0) + 1
        );

        baseline.push({
          user_id: c.id,
          product_id: productId,
          quantity: qty,
          subtotal: price,
          tax,
          discount,
          created_at: when,
        });
      }

      const extra: OrderData[] = [];
      for (const user of insertedUsers) {
        if (!hasOrdersMap.get(user.id)) continue;

        const freq = customerOrderFrequency.get(user.id) ?? 0;
        const orderCount = Math.max(0, freq - 1);

        for (let j = 0; j < orderCount; j++) {
          const qty = paretoRandom(1, 30, 2);
          const price = Number(
            faker.commerce.price({ min: 20, max: 1_500, dec: 2 })
          );
          const tax = Number((price * 0.1).toFixed(2));
          const discount = calculateDiscount(price);
          const when = faker.date.between({
            from: user.created_at,
            to: END,
          });
          const existingLastOrder = lastOrderMap.get(user.id);
          if (!existingLastOrder || when > existingLastOrder) {
            lastOrderMap.set(user.id, when);
          }

          const productIndex = powerLawRandom(0, org.productIds.length - 1, 2);
          const productId = org.productIds[productIndex];
          if (productId !== undefined) {
            productOrderCounts.set(
              productId,
              (productOrderCounts.get(productId) ?? 0) + 1
            );

            extra.push({
              user_id: user.id,
              product_id: productId,
              quantity: qty,
              subtotal: price,
              tax,
              discount,
              created_at: when,
            });
          }
        }
      }

      const orders = baseline.concat(extra);

      await batchInsert(
        client,
        `INSERT INTO public.orders
           (organisation_id,user_id,product_id,quantity,subtotal,tax,discount,created_at)`,
        orders,
        (o) => [
          org.id,
          o.user_id,
          o.product_id,
          o.quantity,
          o.subtotal,
          o.tax,
          o.discount,
          o.created_at,
        ],
        5_000
      );

      // Update last_order_at for users who have orders
      const idsToUpdate: number[] = [];
      const datesToUpdate: Date[] = [];
      for (const c of insertedUsers) {
        const lastOrder = lastOrderMap.get(c.id);
        if (hasOrdersMap.get(c.id) && lastOrder) {
          idsToUpdate.push(c.id);
          datesToUpdate.push(lastOrder);
        }
      }
      await updateLastOrder(client, org.id, idsToUpdate, datesToUpdate);

      // Reviews with skewed distribution
      const reviews: ReviewData[] = [];

      for (let i = 0; i < 5_000; i++) {
        const productIndex = powerLawRandom(0, org.productIds.length - 1, 2);
        const productId = org.productIds[productIndex];
        if (productId === undefined) continue;

        const userIndex = powerLawRandom(0, insertedUsers.length - 1, 1.5);
        const user = insertedUsers[userIndex];
        if (!user) continue;

        reviews.push({
          user_id: user.id,
          product_id: productId,
          rating: faker.number.int({ min: 1, max: 5 }),
          body: faker.lorem.sentences({ min: 1, max: 3 }),
          created_at: faker.date.between({
            from: user.created_at,
            to: END,
          }),
        });
      }

      await batchInsert(
        client,
        `INSERT INTO public.reviews
           (organisation_id,product_id,user_id,rating,comment,created_at)`,
        reviews,
        (r) => [org.id, r.product_id, r.user_id, r.rating, r.body, r.created_at]
      );
    }

    console.log("  Demo data seeded successfully");
  } finally {
    await client.end();
  }
}
