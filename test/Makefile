TESTFILES=$(shell ls -1 *.sig)
RESFILES=$(TESTFILES:%.sig=%.parse.res) $(TESTFILES:%.sig=%.client.res) $(TESTFILES:%.sig=%.defs.res) $(TESTFILES:%.sig=%.server.res)

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
	rm -rf *~ *.out *.res *.defs.sml *.defs.mlb *.client.mlb *.server.mlb *.exe *.client.sml *.server.sml
	find . -name 'MLB' | xargs rm -rf

# -----------------
# PARSING
# -----------------

%.parse.out: %.sig
	../smlexpose -v $< > $@

%.parse.res: %.parse.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $<: OK" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "Test $<: *** ERR: file $< differs from $<.ok ***" > $@ \
             ; else \
                echo "Test $<: *** ERR: file $<.ok does not exist ***" > $@ \
             ; fi \
         ; fi)

# -----------------
# SERVICE DEFS
# -----------------

%.defs.sml: %.sig
	../smlexpose -silent -d $< > $@

%.defs.mlb: %.defs.sml
	@echo 'local $$XSML_LIBY/basis/basis.mlb' | sed -e 's/X/(/g' | sed -e 's/Y/)/g' > $@
	@echo '      pickle/pickle.sig' >> $@
	@echo in >> $@
	@echo $*.sig >> $@
	@echo $< >> $@
	@echo end >> $@

%.defs.res: %.defs.mlb
	@echo "Test $<: ERR" > $@
	-(mlkit -o $*.defs.exe $< && ./$*.defs.exe > /dev/null && echo "Test $<: OK" > $@)

# -----------------
# CLIENT
# -----------------

%.client.sml: %.sig
	../smlexpose -silent -c -url "some_url" $< > $@

%.client.mlb: %.defs.sml %.client.sml
	@echo 'local $$XSML_LIBY/basis/basis.mlb' | sed -e 's/X/(/g' | sed -e 's/Y/)/g' > $@
	@echo '      pickle/pickle.sig' >> $@
	@echo '      async/deferred.sig' >> $@
	@echo '      async/async.sig' >> $@
	@echo in >> $@
	@echo $*.sig >> $@
	@echo $^ >> $@
	@echo end >> $@

%.client.res: %.client.mlb
	@echo "Test $<: ERR" > $@
	-(mlkit -o $*.client.exe $< && ./$*.client.exe > /dev/null && echo "Test $<: OK" > $@)


# -----------------
# SERVER
# -----------------

%.server.sml: %.sig
	../smlexpose -silent -s $< > $@

%.server.mlb: %.defs.sml %.server.sml
	@echo 'local $$XSML_LIBY/basis/basis.mlb' | sed -e 's/X/(/g' | sed -e 's/Y/)/g' > $@
	@echo '      pickle/pickle.sig' >> $@
	@echo '      web/web.sig' >> $@
	@echo in >> $@
	@echo $*.sig >> $@
	@echo $^ >> $@
	@echo end >> $@

%.server.res: %.server.mlb
	@echo "Test $<: ERR" > $@
	-(mlkit -o $*.server.exe $< && ./$*.server.exe > /dev/null && echo "Test $<: OK" > $@)
