#!/usr/bin/env bash

# Build the documentation website (https://tasklite.org) on Netlify.
# Netlify's build image doesn't include mdBook,
# so the prebuilt Linux binaries are downloaded into `./bin` first.

set -euo pipefail

mdbook_version="0.5.1"
mdbook_toc_version="0.15.2"

mdbook_base="https://github.com/rust-lang/mdBook/releases/download"
mdbook_url="$mdbook_base/v$mdbook_version"
mdbook_url="$mdbook_url/mdbook-v$mdbook_version-x86_64-unknown-linux-gnu.tar.gz"

toc_base="https://github.com/badboy/mdbook-toc/releases/download"
toc_url="$toc_base/$mdbook_toc_version"
toc_url="$toc_url/mdbook-toc-$mdbook_toc_version-x86_64-unknown-linux-gnu.tar.gz"

mkdir -p bin

curl --silent --show-error --fail --location "$mdbook_url" \
  | tar --extract --gzip --directory bin

curl --silent --show-error --fail --location "$toc_url" \
  | tar --extract --gzip --directory bin

export PATH="$PWD/bin:$PATH"

mdbook build
