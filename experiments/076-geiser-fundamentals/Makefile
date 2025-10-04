# Makefile for Experiment 076: Geiser Fundamentals

GUILE := guile
EMACS := emacs

.PHONY: all test clean help info install

all: test

help:
	@echo "Experiment 076: Geiser Fundamentals - Make targets"
	@echo ""
	@echo "  make test          - Run Guile tests"
	@echo "  make emacs         - Launch Emacs with Geiser config"
	@echo "  make install       - Install Geiser in Emacs"
	@echo "  make info          - Show experiment information"
	@echo "  make clean         - Remove generated files"

test:
	@echo "Running Geiser example in Guile..."
	@$(GUILE) -l hello-geiser.scm -c '(run-all-tests)'

emacs:
	@echo "Launching Emacs with Geiser configuration..."
	@$(EMACS) -Q -l init-geiser.el hello-geiser.scm &

install:
	@echo "Installing Geiser packages in Emacs..."
	@$(EMACS) --batch --eval "(progn \
	  (require 'package) \
	  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\")) \
	  (package-initialize) \
	  (package-refresh-contents) \
	  (package-install 'geiser) \
	  (package-install 'geiser-guile))"
	@echo "✓ Geiser installed"

info:
	@echo "╔══════════════════════════════════════════════════════════╗"
	@echo "║  Experiment 076: Geiser Fundamentals                    ║"
	@echo "╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Geiser: Generic Environment for Interactive Scheme Evaluation"
	@echo ""
	@echo "Features:"
	@echo "  • REPL integration for multiple Scheme implementations"
	@echo "  • Autodoc (inline documentation)"
	@echo "  • Context-aware completion"
	@echo "  • Jump to definition"
	@echo "  • Interactive debugging"
	@echo ""
	@echo "Files:"
	@echo "  - README.org       (Full documentation)"
	@echo "  - init-geiser.el   (Emacs configuration)"
	@echo "  - hello-geiser.scm (Example Scheme code)"
	@echo ""
	@echo "Quick start:"
	@echo "  1. make install    # Install Geiser packages"
	@echo "  2. make emacs      # Launch Emacs with config"
	@echo "  3. M-x run-guile   # Start Geiser REPL"
	@echo "  4. C-c C-b         # Eval buffer"
	@echo "  5. (run-all-tests) # Run test suite"

clean:
	@find . -name "*.go" -delete
	@find . -name "*~" -delete
	@echo "✓ Clean complete"
