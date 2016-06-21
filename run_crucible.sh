#!/bin/sh

rm -rf crucible-*

stack exec crucible -- tester                                            \
  --build-cmd="stack exec caut-rust-ref -- --spec=%s --output=rust"      \
  --build-cmd="cargo build --manifest-path rust/Cargo.toml"              \
  --build-cmd="touch rust/.projectile"                                   \
  --run-cmd="cargo run --manifest-path rust/Cargo.toml"                  \
  --schema-count=1                                                       \
  --instance-count=100                                                   \
  --type-count=512                                                       \
  --enc-size=1024                                                        \
