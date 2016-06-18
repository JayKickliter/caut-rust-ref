#!/bin/sh

rm -rf crucible-*

stack exec crucible -- tester                                       \
  --build-cmd="stack exec caut-rust-ref -- --spec=%s --output=rust" \
  --build-cmd="cargo build --manifest-path rust/Cargo.toml"         \
  --run-cmd="cat"                                                   \
  --schema-count=1                                                  \
  --instance-count=1000                                             \
  --type-count=10                                                   \
  --enc-size=1024                                                   \
