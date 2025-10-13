import type { Schema } from "~/server/db/schema";

export const stubSchema = [
  {
    name: "User",
    access: "QUERYABLE",
    context: null,
    columns: [
      { name: "name", dbName: "name", rename: null, notes: null, type: "string", unit: null, relation: [] },
      { name: "age", dbName: "age", rename: null, notes: null, type: "number", unit: null, relation: [] },
      { name: "active", dbName: "active", rename: null, notes: null, type: "boolean", unit: null, relation: [] },
      { name: "createdAt", dbName: "createdAt", rename: null, notes: null, type: "DateTime", unit: null, relation: [] },
    ],
    computedColumns: [
      {
        name: "computedScore",
        table: { name: "User" },
        type: "number",
        ast: null,
        unit: null,
      },
    ],
    outwardRelations: [
      {
        name: "profile",
        relationName: "UserToProfile",
        targetTable: { name: "Profile" },
        isList: false,
        selected: true,
      },
      {
        name: "posts",
        relationName: "UserToPosts",
        targetTable: { name: "Post" },
        isList: true,
        selected: true,
      },
    ],
    condition: null,
  },
  {
    name: "Profile",
    access: "QUERYABLE",
    context: null,
    columns: [
      { name: "id", dbName: "id", rename: null, notes: null, type: "string", unit: null, relation: [] },
      { name: "bio", dbName: "bio", rename: null, notes: null, type: "string", unit: null, relation: [] },
      { name: "reputation", dbName: "reputation", rename: null, notes: null, type: "number", unit: null, relation: [] },
    ],
    computedColumns: [],
    outwardRelations: [],
    condition: null,
  },
  {
    name: "Post",
    access: "QUERYABLE",
    context: null,
    columns: [
      { name: "views", dbName: "views", rename: null, notes: null, type: "number", unit: null, relation: [] },
      { name: "published", dbName: "published", rename: null, notes: null, type: "boolean", unit: null, relation: [] },
      { name: "publishedAt", dbName: "publishedAt", rename: null, notes: null, type: "DateTime", unit: null, relation: [] },
      { name: "title", dbName: "title", rename: null, notes: null, type: "string", unit: null, relation: [] },
    ],
    computedColumns: [],
    outwardRelations: [],
    condition: null,
  },
] satisfies Schema;
