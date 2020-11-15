from haskell:8.8.4-buster as builder

workdir tasklite

copy docker-stack.yaml stack.yaml

copy tasklite-core/README.md tasklite-core/README.md
copy tasklite-core/example-config.yaml tasklite-core/example-config.yaml
copy tasklite-core/package.yaml tasklite-core/package.yaml

copy huzzy huzzy

run stack install --only-dependencies tasklite-core

copy tasklite-core tasklite-core

# Needed for retrieving the version number
copy .git .git

run stack install


# Same OS version as the builder image
from debian:buster
run apt-get update && \
    apt-get install -y libgmp10
copy --from=builder /tasklite/tasklite-core/example-config.yaml /root/.config/tasklite/config.yaml
copy --from=builder /root/.local/bin/tasklite /usr/local/bin/tasklite
cmd ["tasklite"]
