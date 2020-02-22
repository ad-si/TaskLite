from haskell:8.6.5 as builder

copy docker-stack.yaml stack.yaml

copy tasklite-core/package.yaml tasklite-core/package.yaml
run stack install --only-dependencies tasklite-core

copy tasklite-core tasklite-core

# Needed for retrieving the version number
copy .git .git

run stack install


from debian:9.9
run apt-get update && \
    apt-get install -y libgmp10
copy --from=builder /tasklite-core/example-config.yaml /root/.config/tasklite/config.yaml
copy --from=builder /root/.local/bin/tasklite /usr/local/bin/tasklite
cmd ["tasklite"]
