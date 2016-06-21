#!/bin/sh

rm -rf crucible-*

stack exec crucible -- tester                                            \
  --build-cmd="stack exec caut-rust-ref -- --spec=%s --output=rust"      \
  --build-cmd="cargo build --manifest-path rust/Cargo.toml"              \
  --run-cmd="cargo run --manifest-path rust/Cargo.toml"                  \
  --schema-count=1                                                       \
  --instance-count=100                                                   \
  --type-count=100                                                       \
  --enc-size=1024                                                        \
