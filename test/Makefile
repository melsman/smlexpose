.PHONY: clean
clean:
	rm -rf *~

.PHONY: test
test:
	make -C .. smlexpose
	../smlexpose -v test01.sig
