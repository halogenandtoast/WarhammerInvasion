# syntax=docker/dockerfile:1.7

# Stage 1: build the Haskell binary with stack.
# Use the official haskell image so stack is preinstalled and ghc can be
# resolved by the snapshot in stack.yaml.
FROM haskell:9.10.1 AS builder

WORKDIR /build

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
       ca-certificates curl gnupg lsb-release \
       libgmp-dev zlib1g-dev pkg-config \
  && install -d /usr/share/postgresql-common/pgdg \
  && curl -fsSLo /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc \
       https://www.postgresql.org/media/keys/ACCC4CF8.asc \
  && echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
       > /etc/apt/sources.list.d/pgdg.list \
  && apt-get update \
  && apt-get install -y --no-install-recommends libpq-dev \
  && rm -rf /var/lib/apt/lists/*

# Pull in just the dependency descriptors first so dependency compilation
# can be cached independently from source changes.
COPY stack.yaml stack.yaml.lock package.yaml WarhammerInvasion.cabal ./
RUN stack setup
RUN stack build --only-dependencies

COPY . .
RUN stack install --local-bin-path /out

# Stage 2: minimal runtime.
FROM debian:bookworm-slim AS runtime

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
       libgmp10 \
       libtinfo6 \
       libpq5 \
       ca-certificates \
       netbase \
  && rm -rf /var/lib/apt/lists/* \
  && useradd --create-home --shell /usr/sbin/nologin whi

USER whi
WORKDIR /home/whi

COPY --from=builder /out/WarhammerInvasion-exe /usr/local/bin/WarhammerInvasion-exe

ENV PORT=3000
EXPOSE 3000

CMD ["WarhammerInvasion-exe"]
