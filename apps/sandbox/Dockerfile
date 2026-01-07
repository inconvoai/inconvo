FROM docker.io/cloudflare/sandbox:0.6.9-python

# On a Mac with Apple Silicon, you might need to specify the platform:
# FROM --platform=linux/arm64 docker.io/cloudflare/sandbox:0.4.3

# Install altair for chart generation (outputs Vega-Lite specs)
RUN --mount=type=cache,target=/root/.cache/pip \
    pip3 install --no-cache-dir altair

