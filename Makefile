.phony: test clean

test:
	cargo test --manifest-path static/cauterize/Cargo.toml
	stack build caut-rust-ref
	sh run_crucible.sh

clean:
	stack clean
	rm -rf crucible-*
	cargo clean --manifest-path static/cauterize/Cargo.toml
