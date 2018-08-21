TESTFILES=$(shell ls -1 *.sig)
OUTFILES=$(TESTFILES:%.sig=%.out)
RESFILES=$(TESTFILES:%.sig=%.res)

EXPOSE_TESTFILES=$(shell ls -1 *.expose.sig)
EXPOSE_SMLFILES=$(EXPOSE_TESTFILES:%.sig=%.sml)
EXPOSE_MLBFILES=$(EXPOSE_TESTFILES:%.sig=%.mlb)
EXPOSE_EXEFILES=$(EXPOSE_TESTFILES:%.sig=%.exe)


.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "-------------------------------------"
	@exit `grep "ERR" $(RESFILES) | wc -l`

.PHONY: test_expose
test_expose: $(EXPOSE_SMLFILES) $(EXPOSE_MLBFILES) $(EXPOSE_EXEFILES)

.PHONY: clean
clean:
	rm -rf *~ *.out *.res $(EXPOSE_SMLFILES) $(EXPOSE_MLBFILES) *.exe
	find . -name 'MLB' | xargs rm -rf

%.out: %.sig
	../smlexpose -v $< > $@

%.expose.sml: %.expose.sig
	../smlexpose -silent -d $< > $@

%.expose.mlb: %.expose.sml
	@echo 'local $$XSML_LIBY/basis/basis.mlb' | sed -e 's/X/(/g' | sed -e 's/Y/)/g' > $@
	@echo '      pickle/pickle.mlb' >> $@
	@echo in >> $@
	@echo $*.expose.sig >> $@
	@echo $< >> $@
	@echo end >> $@

%.expose.exe: %.expose.mlb
	mlkit -o $@ $<

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
