.phony: executable generate

rust/example/schema/rust_test.spec: rust/example/schema/rust_test.scm
	stack exec cauterize -- --schema rust/example/schema/rust_test.scm --specification rust/example/schema/rust_test.spec

executable:
	stack build caut-rust-ref

generate: rust/example/schema/rust_test.spec executable
	stack exec caut-rust-ref -- -s rust/example/schema/rust_test.spec -o rust/example/src

rust-lib-build:
	cargo build --manifest-path rust/cauterize/Cargo.toml

rust-lib-test:
	cargo test --manifest-path rust/cauterize/Cargo.toml

rust-example-build:
	cargo build --manifest-path rust/example/Cargo.toml

rust-example-test:
	cargo test --manifest-path rust/example/Cargo.toml
