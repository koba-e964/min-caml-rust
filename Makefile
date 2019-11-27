%.x: %.asm assets/x86_64/libmincaml.S assets/stub.c
	cc assets/stub.c assets/x86_64/libmincaml.S $*.asm -o $*.x
%.asm: %.ml
	cargo run $< $@
