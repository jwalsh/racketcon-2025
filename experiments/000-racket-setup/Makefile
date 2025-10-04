# Makefile for Experiment 000: Racket and Rosette Setup

RACKET := racket
RACO := raco

.PHONY: all setup test clean help info

all: info

help:
	@echo "Experiment 000: Racket and Rosette Setup - Make targets"
	@echo ""
	@echo "  make setup    - Install Rosette package"
	@echo "  make test     - Verify installation"
	@echo "  make info     - Show experiment information"
	@echo "  make clean    - Remove generated files"

setup:
	@echo "Installing Rosette..."
	@$(RACO) pkg install --auto rosette
	@echo "✓ Rosette installed"

test:
	@echo "Verifying Racket installation..."
	@$(RACKET) --version
	@echo ""
	@echo "Testing basic Racket..."
	@$(RACKET) -e '(displayln "Racket is working!")'
	@echo ""
	@if $(RACO) pkg show rosette >/dev/null 2>&1; then \
		echo "Testing Rosette..."; \
		$(RACKET) verify.rkt; \
		$(RACKET) hello-rosette.rkt; \
	else \
		echo "⚠ Rosette not installed. Run 'make setup' first."; \
	fi

info:
	@echo "╔══════════════════════════════════════════════════════════╗"
	@echo "║  Experiment 000: Racket and Rosette Setup               ║"
	@echo "╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Initial setup for RacketCon 2025 experiments"
	@echo ""
	@echo "Prerequisites:"
	@echo "  • Racket 8.15 (installed: $$(racket --version 2>/dev/null || echo 'NOT FOUND'))"
	@echo "  • Rosette package"
	@echo ""
	@echo "Files:"
	@echo "  - verify.rkt       (Verify Rosette installation)"
	@echo "  - hello-rosette.rkt (Basic Rosette example)"
	@echo ""
	@echo "Quick start:"
	@echo "  1. make setup    # Install Rosette"
	@echo "  2. make test     # Verify everything works"

clean:
	@find . -name "*.zo" -delete
	@find . -name "compiled" -type d -exec rm -rf {} + 2>/dev/null || true
	@echo "✓ Clean complete"
