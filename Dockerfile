FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gnupg \
    unzip \
    build-essential \
    pkg-config \
    libssl-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

ARG DART_SDK_VERSION=2.19.6
RUN curl -fsSL "https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_SDK_VERSION}/sdk/dartsdk-linux-x64-release.zip" \
    -o /tmp/dart-sdk.zip \
    && unzip -q /tmp/dart-sdk.zip -d /usr/local \
    && rm /tmp/dart-sdk.zip

RUN curl https://sh.rustup.rs -sSf \
    | sh -s -- -y --no-modify-path --default-toolchain stable --profile minimal

ENV PATH="/usr/local/dart-sdk/bin:/root/.cargo/bin:${PATH}"

WORKDIR /app

RUN git clone https://github.com/munificent/craftinginterpreters.git

RUN cd craftinginterpreters/tool && dart pub get

COPY Cargo.toml Cargo.lock ./
COPY src ./src

RUN cargo build --release \
    && cp target/release/interpreter /app/interpreter

WORKDIR /app/craftinginterpreters

ENTRYPOINT [ "dart", "tool/bin/test.dart", "jlox", "--interpreter", "../interpreter" ]
