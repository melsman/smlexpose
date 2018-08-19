TESTFILES=$(shell ls -1 *.sig)
OUTFILES=$(TESTFILES:%.sig=%.out)
RESFILES=$(TESTFILES:%.sig=%.res)

.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "-------------------------------------"
	@exit `grep "ERR" $(RESFILES) | wc -l`

.PHONY: clean
clean:
	rm -rf *~ *.out *.res

%.out: %.sig
	../smlexpose -v $< > $@

%.res: %.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*.sig: OK" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "Test $*.sig: *** ERR: file $< differs from $<.ok ***" > $@ \
             ; else \
                echo "Test $*.sig: *** ERR: file $<.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
