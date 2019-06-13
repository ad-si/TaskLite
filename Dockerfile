from haskell:8.6.5 as builder

add tasklite-core tasklite-core
add tasklite-app tasklite-app

add stack.yaml stack.yaml
run stack install tasklite-core

# from alpine:3.9.4
# copy --from=builder /usr/root/.local/bin/tasklite .
