.phony: all generate

generate:
	stack exec caut-rust-ref -- -s priv/rust_test.spec -o priv/rust_test
