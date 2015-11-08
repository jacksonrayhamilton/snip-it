EMACS = emacs

all: test

test:
	${EMACS} -Q -batch \
	-L . \
	-l ert \
	-l test/snip-it-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: all test
