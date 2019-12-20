MKDIR ?= mkdir -vp
CP    ?= cp

.PHONY: \
	all clean \
	lib syntax package \
	test promote clean-tests \
	samples clean-samples

.DEFAULT_GOAL: all

all: package samples

package: lib syntax

lib:
	dune build src/OCanren.cma
	dune build src/OCanren.cmxa

syntax:
	dune build camlp5/pa_ocanren.cma

samples:
	dune build @samples

clean-tests:
	rm -f regression/*.log regression/*.diff

clean-samples:
	rm -f samples/*.log samples/*.diff

clean: clean-samples
	dune clean