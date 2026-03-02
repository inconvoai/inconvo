# @repo/connect

Internal database connector used by the Inconvo agent runtime and server. Built on Kysely and not intended as a public API.

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

- ✅ Support for PostgreSQL, MySQL, MSSQL, and BigQuery
- ✅ Schema introspection and augmentation
- ✅ Query operations for the agent retriever

## Environment Variables

```env
INCONVO_DATABASE_URL=your_database_connection_string
DATABASE_DIALECT=postgresql # or redshift, mysql, mssql
NODE_ENV=development # or production, test
INCONVO_SECRET_KEY=optional_secret_key
INCONVO_BIGQUERY_PROJECT_ID=project-id
INCONVO_BIGQUERY_DATASET=dataset-name
INCONVO_BIGQUERY_LOCATION=EU
INCONVO_BIGQUERY_KEYFILE=/path/to/key.json
```

## Usage

This package is wired into the agent runtime and server. Public usage docs are intentionally minimal; refer to tests for examples and use the agent-facing interfaces in `@repo/agents`.

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
