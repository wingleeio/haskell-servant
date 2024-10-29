FROM haskell:9.6.6 as builder

WORKDIR /app

RUN apt-get update && \
    apt-get install -y libpq-dev && \
    rm -rf /var/lib/apt/lists/*

COPY stack.yaml stack.yaml.lock package.yaml ./

COPY src/ src/
COPY app/ app/
COPY test/ test/
COPY public/ public/

RUN stack build --system-ghc && \
    BINARY_PATH=$(stack path --local-install-root)/bin/haskell-servant-exe && \
    cp $BINARY_PATH /app/haskell-servant-exe && \
    ls -la /app/haskell-servant-exe

FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get install -y libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/haskell-servant-exe ./haskell-servant-exe

COPY public/ ./public/

RUN chmod +x /app/haskell-servant-exe

ENV PORT=8080

EXPOSE 8080

CMD ["/app/haskell-servant-exe"]