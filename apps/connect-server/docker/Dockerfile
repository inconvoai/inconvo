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
ENV NODE_ENV=production
WORKDIR /app

# Copy the bundle
COPY --from=builder /app/apps/connect-server/dist/index.cjs .

# Install only the packages that must be external (native/dynamic require)
# These cannot be bundled by esbuild
RUN npm init -y && npm install --omit=dev \
    pino \
    pino-pretty \
    pg \
    mysql2 \
    tedious \
    @google-cloud/bigquery

CMD ["node", "index.cjs"]
EXPOSE 3006
