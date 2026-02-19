FROM docker.io/cloudflare/sandbox:0.7.4-python

# On a Mac with Apple Silicon, you might need to specify the platform:
# FROM --platform=linux/arm64 docker.io/cloudflare/sandbox:0.4.3

# Install altair for chart generation (outputs Vega-Lite specs)
RUN --mount=type=cache,target=/root/.cache/pip \
    pip3 install --no-cache-dir \
    altair==6.0.0 \
    jsonschema==4.24.1

# Precompile Python bytecode to speed up imports (~10-20% faster)
RUN python3 -m compileall -q /usr/lib/python3* /usr/local/lib/python3* 2>/dev/null || true

# Pre-install inconvo helper module so `from inconvo import ...` works
COPY scripts/inconvo.py /workspace/inconvo.py

