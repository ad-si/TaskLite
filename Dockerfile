# ATTENTION: Also update the version of the final image
FROM haskell:9.8.4-slim-bullseye AS builder

WORKDIR /tasklite

COPY stack.yaml stack.yaml

COPY tasklite/README.md tasklite/README.md
COPY tasklite/package.yaml tasklite/package.yaml

COPY tasklite-core/README.md tasklite-core/README.md
COPY tasklite-core/example-config.yaml tasklite-core/example-config.yaml
COPY tasklite-core/package.yaml tasklite-core/package.yaml

RUN stack install --only-dependencies tasklite-core

########## Install CLI tool ##########

COPY tasklite-core tasklite-core
COPY tasklite tasklite

# Needed for retrieving the version number
COPY .git .git

# Remove after issue https://github.com/commercialhaskell/stack/issues/3348
RUN mkdir -p /etc/stack
RUN echo "allow-different-user: true" >> /etc/stack/config.yaml

RUN stack install tasklite


# Same OS version as the builder image
FROM haskell:9.8.4-slim-bullseye
RUN apt-get update && \
    apt-get install -y libgmp10
COPY --from=builder \
    /tasklite/tasklite-core/example-config.yaml \
    /root/.config/tasklite/config.yaml
COPY --from=builder /root/.local/bin/tasklite /usr/local/bin/tasklite

ENTRYPOINT  ["tasklite"]
