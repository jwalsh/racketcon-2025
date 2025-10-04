# Makefile for Experiment 061: Amazon Ion Format Basics

.PHONY: all help info clean

all: info

help:
	@echo "Experiment 061: Amazon Ion Format - Make targets"
	@echo ""
	@echo "  make info     - Show experiment information"
	@echo "  make view     - View Ion examples"
	@echo "  make clean    - Remove generated files"

info:
	@echo "╔══════════════════════════════════════════════════════════╗"
	@echo "║  Experiment 061: Amazon Ion Format Basics               ║"
	@echo "╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Amazon Ion: A richly-typed, self-describing data format"
	@echo ""
	@echo "Features:"
	@echo "  • Superset of JSON with more types"
	@echo "  • Type annotations"
	@echo "  • Binary and text formats"
	@echo "  • Timestamps, decimals, symbols"
	@echo "  • S-expressions and annotations"
	@echo ""
	@echo "Files:"
	@echo "  - README.org       (Documentation)"
	@echo "  - ion-examples.ion (Example Ion data)"
	@echo ""
	@echo "Ion Types:"
	@echo "  • null, bool, int, float, decimal"
	@echo "  • timestamp, string, symbol, blob, clob"
	@echo "  • list, sexp, struct"
	@echo ""
	@echo "Note: Ion processing requires ion-java or ion-python"
	@echo "This experiment focuses on format documentation"

view:
	@echo "Ion Examples:"
	@cat ion-examples.ion

clean:
	@rm -f *.log
	@echo "✓ Clean complete"
