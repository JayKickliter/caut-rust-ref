#!/bin/sh

stack exec crucible -- tester                                                                \
  --build-cmd="stack exec caut-rust-ref -- --spec=%s --output=rust"                          \
  --build-cmd="cargo build --manifest-path rust/Cargo.toml"                                  \
  --run-cmd="tee rust/data.bin | RUST_BACKTRACE=1 cargo run --manifest-path rust/Cargo.toml" \
  --schema-count=10                                                                          \
  --instance-count=100                                                                       \
  --type-count=10                                                                            \
  --enc-size=1048576                                                                         \
