# @ten-dev/inconvo

A database abstraction package that supports MSSQL, PostgreSQL, and MySQL without Drizzle dependencies. Built on top of Kysely for type-safe SQL queries.

## Implementation Status

### ✅ Completed Features
- Database connections for PostgreSQL, MySQL, and MSSQL
- Schema introspection with foreign key extraction
- Where condition builder supporting complex nested conditions
- Computed columns support with SQL AST parsing
- All core operations implemented:
  - findMany
  - findDistinct
  - findDistinctByEditDistance
  - count
  - countRelations
  - aggregate
  - groupBy (supports column, date interval, and date component keys)
- Express.js router with same API as original package
- Next.js route handlers

### ⚠️ Partial Implementation
- Complex relation handling in findMany (simplified version)
- MSSQL connection configuration (needs proper tedious setup)
- JSON column support (basic implementation)
- CTE-based relation queries (not fully implemented)

### 🔧 Known Issues
- TypeScript build errors with dynamic selections (runtime works)
- Some relation operators (some/none/every) not fully implemented
- Edit distance for databases without native support uses JS fallback

## Features

- ✅ Support for PostgreSQL, MySQL, and MSSQL
- ✅ Database introspection and schema extraction
- ✅ Type-safe query building with Kysely
- ✅ Express.js middleware integration
- ✅ Next.js route handler support
- ✅ Aggregation operations

## Installation

```bash
npm install @ten-dev/inconvo
```

## Environment Variables

```env
INCONVO_DATABASE_URL=your_database_connection_string
DATABASE_DIALECT=postgresql # or redshift, mysql, mssql
NODE_ENV=development # or production, test
INCONVO_SECRET_KEY=optional_secret_key
```

## Usage

### Basic Setup

```typescript
import { getDb, buildSchema } from '@ten-dev/inconvo';

// Get database connection
const db = await getDb();

// Get schema information
const schema = await buildSchema();
console.log(schema.tables);
```

### Query Operations

```typescript
import { executeQuery, executeAggregate } from '@ten-dev/inconvo';

// Simple query
const users = await executeQuery({
  table: 'users',
  select: ['id', 'name', 'email'],
  where: { active: true },
  orderBy: [{ column: 'created_at', order: 'desc' }],
  limit: 10
});

// Aggregation
const userCount = await executeAggregate({
  table: 'users',
  operation: 'count',
  where: { active: true }
});

// With joins
const ordersWithUsers = await executeQuery({
  table: 'orders',
  select: ['orders.id', 'orders.total', 'users.name'],
  joins: [{
    table: 'users',
    on: { left: 'orders.user_id', right: 'users.id' },
    type: 'inner'
  }]
});
```

### Mutations

```typescript
import { executeInsert, executeUpdate, executeDelete } from '@ten-dev/inconvo';

// Insert
const newUser = await executeInsert({
  table: 'users',
  data: {
    name: 'John Doe',
    email: 'john@example.com'
  },
  returning: ['id']
});

// Update
const updated = await executeUpdate({
  table: 'users',
  data: { active: false },
  where: { id: 1 },
  returning: ['id', 'active']
});

// Delete
const deleted = await executeDelete({
  table: 'users',
  where: { id: 1 }
});
```

### Express Integration

```typescript
import express from 'express';
import { inconvoMiddleware, createInconvoRouter } from '@ten-dev/inconvo/express';

const app = express();

// Use as middleware
app.use(inconvoMiddleware);

// Or use the router
app.use('/api/db', createInconvoRouter());

// Access in routes
app.get('/users', async (req, res) => {
  const users = await req.inconvo.query({
    table: 'users',
    select: ['id', 'name']
  });
  res.json(users);
});
```

### Next.js Integration

```typescript
// app/api/db/route.ts
export { GET, POST } from '@ten-dev/inconvo/next';

// Or custom implementation
import { createInconvoRoute } from '@ten-dev/inconvo/next';

const handlers = createInconvoRoute();
export const GET = handlers.GET;
export const POST = handlers.POST;
```

## Database Connection Strings

### PostgreSQL
```
postgresql://user:password@localhost:5432/database
```

### Amazon Redshift
```
postgresql://user:password@cluster-name.region.redshift.amazonaws.com:5439/database?sslmode=require
```

### MySQL
```
mysql://user:password@localhost:3306/database
```

### MSSQL
```
mssql://user:password@localhost:1433/database?encrypt=true&trustServerCertificate=true
```

## API Reference

### Query Options

- `table`: Target table name
- `select`: Array of columns to select (optional, defaults to all)
- `where`: Where conditions as key-value pairs
- `orderBy`: Array of order specifications
- `limit`: Result limit
- `offset`: Result offset
- `joins`: Array of join specifications

### Aggregate Options

- `table`: Target table name
- `operation`: 'count' | 'sum' | 'avg' | 'min' | 'max'
- `column`: Column to aggregate (optional for count)
- `where`: Where conditions
- `groupBy`: Columns to group by

## License

UNLICENSED
