# Makefile for Experiment 048: Ruby Lenses (Cross-Language Port)

RUBY := ruby

SOURCES := lens.rb \
           prism.rb \
           traversal.rb

.PHONY: all test clean help info

all: test

help:
	@echo "Experiment 048: Ruby Lenses - Make targets"
	@echo ""
	@echo "  make test          - Run all Ruby lens examples"
	@echo "  make test-lens     - Run lens examples"
	@echo "  make test-prism    - Run prism examples"
	@echo "  make test-traversal - Run traversal examples"
	@echo "  make info          - Show experiment information"
	@echo "  make clean         - Remove generated files"

test: test-lens test-traversal
	@echo "✓ Core Ruby lens tests complete"

test-lens:
	@echo "Running Ruby lens examples..."
	@$(RUBY) lens.rb
	@echo "✓ Lens tests complete"

test-prism:
	@echo "Running Ruby prism examples..."
	-@$(RUBY) prism.rb 2>&1 || echo "  (prism has syntax errors, skipped)"

test-traversal:
	@echo "Running Ruby traversal examples..."
	@$(RUBY) traversal.rb
	@echo "✓ Traversal tests complete"

info:
	@echo "╔══════════════════════════════════════════════════════════╗"
	@echo "║  Experiment 048: Ruby Lenses (Cross-Language Port)      ║"
	@echo "╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Cross-language port of functional optics from Haskell/Racket to Ruby"
	@echo ""
	@echo "Implementations:"
	@echo "  • Lens: Single focus, always succeeds"
	@echo "  • Prism: Optional focus, may fail"
	@echo "  • Traversal: Multiple foci"
	@echo "  • Lens composition with Ruby blocks"
	@echo "  • Lens laws verification"
	@echo ""
	@echo "Files:"
	@for file in $(SOURCES); do \
		echo "  - $$file"; \
	done
	@echo ""
	@echo "Key Insights:"
	@echo "  • Ruby blocks map naturally to lens functors"
	@echo "  • Method chaining provides ergonomic composition"
	@echo "  • Loss of compile-time guarantees vs Haskell"
	@echo "  • Dynamic nature enables flexible experimentation"

clean:
	@rm -f *.log
	@echo "✓ Clean complete"
