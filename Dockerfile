from haskell:9.2.2-buster as builder

workdir tasklite

copy docker-stack.yaml stack.yaml

copy huzzy huzzy

copy tasklite-core/README.md tasklite-core/README.md
copy tasklite-core/example-config.yaml tasklite-core/example-config.yaml
copy tasklite-core/package.yaml tasklite-core/package.yaml

copy tasklite-server/package.yaml tasklite-server/package.yaml

run stack install --only-dependencies tasklite-core

run stack install --only-dependencies tasklite-server


########## Install CLI tool ##########

copy tasklite-core tasklite-core

# Needed for retrieving the version number
copy .git .git

# Remove after issue https://github.com/commercialhaskell/stack/issues/3348
run mkdir -p /etc/stack
run echo "allow-different-user: true" >> /etc/stack/config.yaml

run stack install tasklite-core


########## Install Server ##########

copy tasklite-server tasklite-server

run stack install tasklite-server


# Same OS version as the builder image
from debian:buster
run apt-get update && \
    apt-get install -y libgmp10
copy --from=builder /tasklite/tasklite-core/example-config.yaml /root/.config/tasklite/config.yaml
copy --from=builder /root/.local/bin/tasklite /usr/local/bin/tasklite
copy --from=builder /root/.local/bin/tasklite-server /usr/local/bin/tasklite-server

cmd  ["tasklite"]
