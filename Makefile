.phony: executable generate

rust/tests/simple.spec: rust/tests/simple.scm
	stack exec cauterize -- --schema rust/tests/simple.scm --specification rust/tests/simple.spec

executable:
	stack build caut-rust-ref

generate: rust/tests/simple.spec executable
	stack exec caut-rust-ref -- -s rust/tests/simple.spec -o test

rust-lib:
	cargo build

test-rust:
	cargo test --manifest-path test/Cargo.toml -- --nocapture

test: test-rust

clean:
	stack clean
	cargo clean
