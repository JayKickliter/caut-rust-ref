.phony: executable test

executable:
	stack build caut-rust-ref

test:
	sh run_crucible.sh

clean:
	stack clean
	rm -rf crucible-*
