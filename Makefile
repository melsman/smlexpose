MLCOMP ?= mlkit

FILES=flags.sml flags.mlb util.sig util.sml util.mlb \
  PARSE_COMB.sig ParseComb.sml REGION.sig Region.sml \
  SmlLex.sml SmlAst.sml SML_PARSE.sig SmlParse.sml expose.sml Main.sml

GIT_VERSION := $(shell git --no-pager describe --tags --always --dirty)
GIT_DATE := $(firstword $(shell git --no-pager show --date=short --format="%ad" --name-only))
PLATFORM := $(shell uname -pmrs)

.PHONY: all
all: smlexpose

BIN_PREFIX ?= /usr/local
BINDIR=$(DESTDIR)$(BIN_PREFIX})bin

.PHONY: install
install: smlexpose
	cp -p $< $(BINDIR)/

# Use temp file src/version~ to ensure regeneration of src/version.sml
# whenever (and only when) the git version changes...
.PHONY: force
version~: force
	@echo '$(GIT_VERSION) $(GIT_DATE)' | cmp -s - $@ || echo '$(GIT_VERSION) $(GIT_DATE)' > $@

version.sml: version~
	@echo "structure Version = struct\n\
   val version = \"$(GIT_VERSION)\"\n\
   val date = \"$(GIT_DATE)\"\n\
   val platform = \"$(PLATFORM)\"\nend" > $@
	@echo Generated file $@
	@echo Git version $(GIT_VERSION) $(GIT_DATE)

smlexpose: smlexpose.mlb $(FILES) version.sml
	$(MLCOMP) -output $@ $<

DISTPOSTFIX?=linux
DISTNAME=smlexpose-$(DISTPOSTFIX)

.PHONY: dist
dist:
	rm -rf dist
	mkdir dist
	mkdir dist/$(DISTNAME)
	mkdir dist/$(DISTNAME)/bin
	cp -p smlexpose dist/$(DISTNAME)/bin/
	cp -p MIT_LICENSE.md dist/$(DISTNAME)/doc/MIT_LICENSE
	cp -p README.md dist/$(DISTNAME)/doc/README.md
	(cd dist; tar -czf $(DISTNAME).tgz $(DISTNAME))

.PHONY: test
test: smlexpose Makefile
	$(MAKE) -C test test

.PHONY: clean
clean: Makefile
	find . -name '*~' | xargs rm -f
	find . -name 'MLB' | xargs rm -rf
	find . -name 'run' | xargs rm -f
	rm -f smlexpose src/version.sml
	$(MAKE) -C test clean
