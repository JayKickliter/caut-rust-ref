.phony: executable generate

rust/tests/simple.spec: rust/tests/simple.scm
	stack exec cauterize -- --schema rust/tests/simple.scm --specification rust/tests/simple.spec

executable:
	stack build caut-rust-ref

generate: rust/tests/simple.spec executable
	stack exec caut-rust-ref -- -s rust/tests/simple.spec -o rust/tests/

rust-lib-build:
	cargo build --manifest-path rust/cauterize/Cargo.toml

rust-lib-test:
	cargo test --manifest-path rust/cauterize/Cargo.toml

rust-example-build:
	cargo build --manifest-path rust/example/Cargo.toml

rust-example-test:
	cargo test --manifest-path rust/example/Cargo.toml
