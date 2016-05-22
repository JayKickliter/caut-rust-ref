.phony: executable generate

priv/rust_test.spec: priv/rust_test.scm
	stack exec cauterize -- --schema priv/rust_test.scm --specification priv/rust_test.spec

executable:
	stack build caut-rust-ref

generate: priv/rust_test.spec executable
	stack exec caut-rust-ref -- -s priv/rust_test.spec -o priv

rustlib-build:
	cargo build --manifest-path rust/cauterize/Cargo.toml

rustlib-test:
	cargo test --manifest-path rust/cauterize/Cargo.toml
