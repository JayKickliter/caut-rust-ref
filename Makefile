.phony: test clean

test:
	stack build caut-rust-ref
	cargo test --manifest-path static/cauterize/Cargo.toml
	sh run_crucible.sh

clean:
	stack clean
	rm -rf crucible-*
	cargo clean --manifest-path static/cauterize/Cargo.toml
