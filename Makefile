EMACS ?= emacs
EMACS_VERSION=$(shell $(EMACS) -batch -eval '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
EMACS_BATCH=$(EMACS) --batch -Q
TABULAR_ELC=tabular.$(EMACS_VERSION).elc

.PHONY: test-compiled test-uncompiled compile

# dist:
# 	cask package
#
test:
	sh test.sh

compile:
	$(EMACS_BATCH) -l dash -f batch-byte-compile tabular.el && mv tabular.elc $(TABULAR_ELC)

$(TABULAR_ELC): tabular.el
	make compile

#emacs -batch -l ert -l tests/tabular-test.el -f ert-run-tests-batch-and-exit

# test-uncompiled:
# 	cask exec ert-runner -l tabular.el

# test-compiled: $(TABULAR_ELC)
# 	cask exec ert-runner -l $(TABULAR_ELC)

# test: test-uncompiled test-compiled

# tryout:
# 	cask exec $(EMACS) -Q -L . -l init-tryout.el test-arx.el
