# Makefile for Experiment 046: Pattern Matching Fundamentals

RACKET := racket

SOURCES := patterns-comprehensive.rkt \
           pattern-grammar.rkt \
           syntax-spec-grammar.rkt \
           define-dsl-syntax.rkt \
           dsl-compilation.rkt

.PHONY: all test clean help info

all: test

help:
	@echo "Experiment 046: Pattern Matching - Make targets"
	@echo ""
	@echo "  make test     - Run all pattern matching examples"
	@echo "  make quick    - Run comprehensive patterns only"
	@echo "  make info     - Show experiment information"
	@echo "  make clean    - Remove generated files"

test:
	@echo "Running pattern matching examples..."
	@$(RACKET) patterns-comprehensive.rkt
	@echo "✓ Pattern matching tests complete"

quick:
	@$(RACKET) patterns-comprehensive.rkt

info:
	@echo "╔══════════════════════════════════════════════════════════╗"
	@echo "║  Experiment 046: Pattern Matching Fundamentals           ║"
	@echo "╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Related Session: Saturday 10:45am EDT"
	@echo "  'A Match-Like DSL for Deep Immutable Updates'"
	@echo ""
	@echo "Implementations:"
	@echo "  • Pattern deconstruction and binding"
	@echo "  • List and struct patterns"
	@echo "  • DSL grammar with syntax-spec"
	@echo "  • Compilation pipeline"
	@echo ""
	@echo "Files:"
	@for file in $(SOURCES); do \
		echo "  - $$file"; \
	done

clean:
	@find . -name "*.zo" -delete
	@find . -name "compiled" -type d -exec rm -rf {} + 2>/dev/null || true
	@echo "✓ Clean complete"
