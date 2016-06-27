#!/bin/sh

rm -rf crucible-*

stack exec crucible -- tester                                                                \
  --build-cmd="stack exec caut-rust-ref -- --spec=%s --output=rust"                          \
  --build-cmd="cargo build --manifest-path rust/Cargo.toml"                                  \
  --run-cmd="tee rust/data.bin | RUST_BACKTRACE=1 cargo run --manifest-path rust/Cargo.toml" \
  --schema-count=1                                                                           \
  --instance-count=1000                                                                      \
  --type-count=100                                                                           \
  --enc-size=524288                                                                          \
