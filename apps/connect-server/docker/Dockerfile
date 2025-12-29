###################
# PRUNE STAGE
###################
FROM node:22-alpine AS pruner
RUN corepack enable && corepack prepare pnpm@10.26.0 --activate
ENV PNPM_HOME="/root/.local/share/pnpm"
ENV PATH="${PNPM_HOME}:${PATH}"
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
RUN pnpm --filter connect-server run build

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

# Copy workspace package.json files and source
COPY --from=builder /app/packages/connect/package.json packages/connect/
COPY --from=builder /app/packages/connect/src packages/connect/src
COPY --from=builder /app/packages/types/package.json packages/types/
COPY --from=builder /app/packages/types/src packages/types/src
COPY --from=builder /app/apps/connect-server/package.json apps/connect-server/
COPY --from=builder /app/apps/connect-server/dist apps/connect-server/dist

# Install production dependencies only
RUN pnpm install --prod --frozen-lockfile --ignore-scripts

WORKDIR /app/apps/connect-server
CMD ["pnpm", "run", "start:prod"]
EXPOSE 3006
