EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch \
	--eval "(setq load-prefer-newer t)" \
	--eval "(require 'package)" \
	--eval "(package-initialize t)" \
	--eval "(dolist (pkg '(transient toml tomelr)) (package-activate pkg))"

LOADPATH = -L .

BYTE_COMPILE_FLAGS = --eval "(setq byte-compile-error-on-warn nil)"

SRCS = agent-switch-authinfo.el agent-switch-core.el agent-switch-storage.el agent-switch-adapters.el agent-switch-operations.el agent-switch-ui.el agent-switch.el
COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean test test-unit help

all: compile

help:
	@echo "agent-switch.el"
	@echo ""
	@echo "Targets:"
	@echo "  compile    - Byte compile Elisp files"
	@echo "  test       - Run unit tests"
	@echo "  test-unit  - Run batch unit tests"
	@echo "  clean      - Remove compiled files"
	@echo "  help       - Show this help message"

compile: $(COMPILED)
	@echo "Compilation complete: $(words $(COMPILED)) files"

%.elc: %.el
	@echo "Compiling $<..."
	@out=$$($(EMACS_BATCH) $(LOADPATH) $(BYTE_COMPILE_FLAGS) -f batch-byte-compile $< 2>&1); \
	status=$$?; \
	printf "%s\n" "$$out" | grep -v "^Compiling" | grep -v "^Wrote" || true; \
	exit $$status

test: test-unit

test-unit:
	@echo "Running unit tests..."
	@$(EMACS_BATCH) $(LOADPATH) \
		-l ert \
		-l agent-switch.el \
		-l test/agent-switch-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning generated files..."
	@rm -f $(COMPILED)
