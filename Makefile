EMACS ?= emacs

.PHONY: test byte-compile clean lint checkdoc compile-strict qa smoke-web test-web

# Run all ERT tests via our test runner
test:
	$(EMACS) -Q --batch -L lisp -l ert -l test/ert-runner.el -f ert-run-tests-batch-and-exit

# Byte-compile sources (warnings allowed)
byte-compile:
	$(EMACS) -Q --batch -L lisp -f batch-byte-compile lisp/*.el

# Checkdoc for Elisp (style/docstrings)
checkdoc:
	$(EMACS) -Q --batch -l scripts/checkdoc-elisp.el -f carriage-checkdoc-elisp-lint

# Lint with package-lint (installs from MELPA in batch)
lint:
	$(EMACS) -Q --batch -l scripts/lint.el -f carriage-package-lint-batch

# Byte-compile with errors on warnings (strict)
compile-strict:
	$(EMACS) -Q --batch -l scripts/compile-strict.el -f carriage-byte-compile-strict

# Clean artifacts
clean:
	rm -f lisp/*.elc carriage-autoloads.el lisp/*autoloads.el

# Full QA sweep
qa: clean byte-compile test lint checkdoc compile-strict

# Smoke test (curl against a running Emacs daemon)
smoke-web:
	@echo "Running smoke web test with curl..."
	@bash scripts/smoke-web.sh

# Run only web-related ERT tests (selector: carriage-web-)
test-web:
	ERT_SELECTOR='carriage-web-' $(EMACS) -Q --batch -L lisp -l ert -l test/ert-runner.el -f ert-run-tests-batch-and-exit
