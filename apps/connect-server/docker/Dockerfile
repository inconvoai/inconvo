###################
# PRUNE STAGE
###################
FROM node:22-alpine AS pruner
RUN corepack enable && corepack prepare pnpm@10.26.0 --activate
RUN pnpm add -g turbo
WORKDIR /app
COPY . .
RUN turbo prune connect-server --docker

###################
# BUILD STAGE
###################
FROM node:22-alpine AS builder
RUN corepack enable && corepack prepare pnpm@10.26.0 --activate
WORKDIR /app

# Install dependencies first (better caching)
COPY --from=pruner /app/out/json/ .
RUN pnpm install --frozen-lockfile

# Copy source and build
COPY --from=pruner /app/out/full/ .
RUN pnpm --filter @ten-dev/inconvo build

###################
# PRODUCTION STAGE
###################
FROM node:22-alpine AS production
RUN corepack enable && corepack prepare pnpm@10.26.0 --activate
ENV NODE_ENV=production
WORKDIR /app

# Copy package files
COPY --from=builder /app/package.json .
COPY --from=builder /app/pnpm-lock.yaml .
COPY --from=builder /app/pnpm-workspace.yaml .
COPY --from=builder /app/.npmrc .

# Copy workspace package.json files
COPY --from=builder /app/packages/connect/package.json packages/connect/
COPY --from=builder /app/apps/connect-server/package.json apps/connect-server/

# Copy built dist and source
COPY --from=builder /app/packages/connect/dist packages/connect/dist
COPY --from=builder /app/apps/connect-server/src apps/connect-server/src

# Install production dependencies only
RUN pnpm install --prod --frozen-lockfile --ignore-scripts

WORKDIR /app/apps/connect-server
CMD ["pnpm", "run", "start"]
EXPOSE 3006
