.phony: executable generate

rust/tests/simple.spec: rust/tests/simple.scm
	stack exec cauterize -- --schema rust/tests/simple.scm --specification rust/tests/simple.spec

executable:
	stack build caut-rust-ref

generate: rust/tests/simple.spec executable
	stack exec caut-rust-ref -- -s rust/tests/simple.spec -o rust/tests/

rust-lib:
	cargo build

rust-test:
	cargo test -- --nocapture

test: test-rust

clean:
	stack clean
	cargo clean
