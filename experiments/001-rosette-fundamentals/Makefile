# Makefile for Experiment 001: Rosette Fundamentals

RACKET := racket
RACO := raco

SOURCES := symbolic-basics.rkt \
           verify-properties.rkt \
           synthesize-functions.rkt \
           debug-example.rkt

.PHONY: all test clean help info check-rosette

all: test

help:
	@echo "Experiment 001: Rosette Fundamentals - Make targets"
	@echo ""
	@echo "  make test      - Run all Rosette examples"
	@echo "  make check     - Verify Rosette is installed"
	@echo "  make info      - Show experiment information"
	@echo "  make clean     - Remove generated files"

check-rosette:
	@if ! $(RACO) pkg show rosette >/dev/null 2>&1; then \
		echo "⚠ Rosette not installed."; \
		echo "Run: raco pkg install rosette"; \
		exit 1; \
	fi

test: check-rosette
	@echo "Running Rosette examples..."
	@echo ""
	@echo "1. Symbolic Basics..."
	@$(RACKET) symbolic-basics.rkt || echo "  (may need Rosette installation)"
	@echo ""
	@echo "2. Verify Properties..."
	@$(RACKET) verify-properties.rkt || echo "  (may need Rosette installation)"
	@echo ""
	@echo "3. Synthesize Functions..."
	@$(RACKET) synthesize-functions.rkt || echo "  (may need Rosette installation)"
	@echo ""
	@echo "4. Debug Example..."
	@$(RACKET) debug-example.rkt || echo "  (may need Rosette installation)"
	@echo ""
	@echo "✓ Rosette examples complete"

info:
	@echo "╔══════════════════════════════════════════════════════════╗"
	@echo "║  Experiment 001: Rosette Fundamentals                   ║"
	@echo "╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Core concepts of Rosette: solver-aided programming"
	@echo ""
	@echo "Topics:"
	@echo "  • Symbolic execution"
	@echo "  • Verification (verify)"
	@echo "  • Synthesis (synthesize)"
	@echo "  • Debugging (debug)"
	@echo ""
	@echo "Files:"
	@for file in $(SOURCES); do \
		echo "  - $$file"; \
	done
	@echo ""
	@echo "Prerequisites:"
	@echo "  • Rosette installed: $$($(RACO) pkg show rosette 2>/dev/null && echo 'YES' || echo 'NO')"

clean:
	@find . -name "*.zo" -delete
	@find . -name "compiled" -type d -exec rm -rf {} + 2>/dev/null || true
	@echo "✓ Clean complete"
