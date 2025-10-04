# Makefile for Experiment 047: Ocular Patdown (Mike Delmonaco's Optics Library)

RACKET := racket

SOURCES := lens-examples.rkt \
           prism-deep-dive.rkt \
           traversal-examples.rkt

.PHONY: all test clean help info install-haskell

all: test

help:
	@echo "Experiment 047: Ocular Patdown - Make targets"
	@echo ""
	@echo "  make test             - Run all ocular-patdown examples"
	@echo "  make test-lenses      - Run lens examples"
	@echo "  make test-prisms      - Run prism examples"
	@echo "  make test-traversals  - Run traversal examples"
	@echo "  make install-haskell  - Install Haskell (for comparison)"
	@echo "  make info             - Show experiment information"
	@echo "  make clean            - Remove generated files"

test: test-lenses test-prisms test-traversals
	@echo "✓ All ocular-patdown tests complete"

test-lenses:
	@echo "Running lens examples..."
	@$(RACKET) lens-examples.rkt
	@echo "✓ Lens examples complete"

test-prisms:
	@echo "Running prism examples..."
	@$(RACKET) prism-deep-dive.rkt
	@echo "✓ Prism examples complete"

test-traversals:
	@echo "Running traversal examples..."
	@$(RACKET) traversal-examples.rkt
	@echo "✓ Traversal examples complete"

install-haskell:
	@echo "Installing Haskell (for lens library comparison)..."
	@bash install-haskell.sh

info:
	@echo "╔══════════════════════════════════════════════════════════╗"
	@echo "║  Experiment 047: Ocular Patdown (Optics Library)        ║"
	@echo "╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Mike Delmonaco's Racket optics library"
	@echo ""
	@echo "Implementations:"
	@echo "  • Lenses (single focus, always succeeds)"
	@echo "  • Prisms (optional focus, may fail)"
	@echo "  • Traversals (multiple foci)"
	@echo "  • Lens composition"
	@echo ""
	@echo "Files:"
	@for file in $(SOURCES); do \
		echo "  - $$file"; \
	done
	@echo ""
	@echo "Documentation:"
	@echo "  - ocular-notes.org (Racket library notes)"
	@echo "  - haskell-lens-notes.org (Haskell comparison)"

clean:
	@find . -name "*.zo" -delete
	@find . -name "compiled" -type d -exec rm -rf {} + 2>/dev/null || true
	@echo "✓ Clean complete"
