# Build stage
FROM node:22-alpine AS builder
RUN corepack enable && corepack prepare pnpm@latest --activate
WORKDIR /usr/src/inconvo-connect

# Copy package files
COPY pnpm-workspace.yaml .
COPY package.json .
COPY pnpm-lock.yaml* .
COPY .npmrc .

# Copy workspace package.json files
COPY packages/connect/package.json packages/connect/
COPY apps/server/package.json apps/server/

# Install dependencies
RUN pnpm install --frozen-lockfile

# Copy source code
COPY packages/connect packages/connect
COPY apps/server apps/server

# Build packages
RUN pnpm --filter @ten-dev/inconvo build

# Production stage
FROM node:22-alpine
RUN corepack enable && corepack prepare pnpm@latest --activate
ENV NODE_ENV=production
WORKDIR /usr/src/inconvo-connect

# Copy built application
COPY --from=builder /usr/src/inconvo-connect/pnpm-workspace.yaml .
COPY --from=builder /usr/src/inconvo-connect/package.json .
COPY --from=builder /usr/src/inconvo-connect/pnpm-lock.yaml* .
COPY --from=builder /usr/src/inconvo-connect/.npmrc .

# Copy package files
COPY --from=builder /usr/src/inconvo-connect/packages/connect/package.json packages/connect/
COPY --from=builder /usr/src/inconvo-connect/apps/server/package.json apps/server/

# Copy built dist
COPY --from=builder /usr/src/inconvo-connect/packages/connect/dist packages/connect/dist
COPY --from=builder /usr/src/inconvo-connect/apps/server apps/server

# Install production dependencies only
RUN pnpm install --prod --frozen-lockfile

WORKDIR /usr/src/inconvo-connect/apps/server
CMD ["pnpm", "run", "start:prod"]
EXPOSE 3006
