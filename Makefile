%.x: %.S assets/x86_64/libmincaml.S assets/stub.c
	cc assets/stub.c assets/x86_64/libmincaml.S $*.S -o $*.x
%.S: %.ml
	cargo run $< $@
