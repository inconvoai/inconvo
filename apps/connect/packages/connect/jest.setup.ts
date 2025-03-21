import dotenv from "dotenv";
dotenv.config({ path: ".env.test" });

jest.mock("@t3-oss/env-core", () => ({
  createEnv: jest.fn(() => ({
    INCONVO_DATABASE_URL: process.env.INCONVO_DATABASE_URL,
    INCONVO_SECRET_KEY: process.env.INCONVO_SECRET_KEY,
    NODE_ENV: process.env.NODE_ENV,
    DATABASE_DIALECT: process.env.DATABASE_DIALECT,
  })),
}));
