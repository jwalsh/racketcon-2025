# Makefile for RacketCon 2025 experiments
# Use gmake (GNU Make) not BSD make

.PHONY: help download-papers clean-papers rosette-paper roulette-paper lint-org
.PHONY: check-deps check-racket check-emacs check-direnv setup resources test

# Default target
help:
	@echo "RacketCon 2025 - Available targets:"
	@echo ""
	@echo "Setup & Dependencies:"
	@echo "  setup           - Initial setup: check deps and download resources"
	@echo "  check-deps      - Check for racket, emacs, direnv"
	@echo "  check-racket    - Verify Racket installation"
	@echo "  check-emacs     - Verify Emacs 30+ installation"
	@echo "  check-direnv    - Check for direnv (optional)"
	@echo ""
	@echo "Resources:"
	@echo "  resources       - Download all resources (papers, etc.)"
	@echo "  download-papers - Download all research papers"
	@echo "  rosette-paper   - Download Rosette PLDI 2014 paper"
	@echo "  roulette-paper  - Download Roulette paper (Cameron Moy)"
	@echo "  clean-papers    - Remove downloaded papers"
	@echo ""
	@echo "Testing:"
	@echo "  test            - Run ERT tests"
	@echo ""
	@echo "Development:"
	@echo "  lint-org        - Lint all org files with org-lint"
	@echo "  help            - Show this help message"
	@echo ""

# Papers directory
PAPERS_DIR := papers

# Create papers directory
$(PAPERS_DIR):
	mkdir -p $(PAPERS_DIR)

# Rosette PLDI 2014 paper
ROSETTE_PAPER := $(PAPERS_DIR)/rosette-pldi2014.pdf
ROSETTE_URL := https://homes.cs.washington.edu/~bodik/ucb/Files/2014/rosette-pldi2014.pdf

$(ROSETTE_PAPER): $(PAPERS_DIR)
	@echo "Downloading Rosette PLDI 2014 paper..."
	curl -L -o $(ROSETTE_PAPER) $(ROSETTE_URL) || fetch -o $(ROSETTE_PAPER) $(ROSETTE_URL)
	@echo "Downloaded: $(ROSETTE_PAPER)"

rosette-paper: $(ROSETTE_PAPER)

# Roulette paper (Cameron Moy - RacketCon 2025)
ROULETTE_PAPER := $(PAPERS_DIR)/roulette.pdf
ROULETTE_URL := https://ccs.neu.edu/~camoy/pub/roulette.pdf

$(ROULETTE_PAPER): $(PAPERS_DIR)
	@echo "Downloading Roulette paper..."
	curl -L -o $(ROULETTE_PAPER) $(ROULETTE_URL) || fetch -o $(ROULETTE_PAPER) $(ROULETTE_URL)
	@echo "Downloaded: $(ROULETTE_PAPER)"

roulette-paper: $(ROULETTE_PAPER)

# Additional papers can be added here
# Example:
# TRUFFLE_PAPER := $(PAPERS_DIR)/truffle-paper.pdf
# TRUFFLE_URL := https://example.com/truffle.pdf
#
# $(TRUFFLE_PAPER): $(PAPERS_DIR)
# 	curl -L -o $(TRUFFLE_PAPER) $(TRUFFLE_URL)

# Download all papers
download-papers: rosette-paper roulette-paper
	@echo "All papers downloaded to $(PAPERS_DIR)/"

# Alias for resources
resources: download-papers
	@echo "All resources downloaded"

# Clean papers directory
clean-papers:
	rm -rf $(PAPERS_DIR)
	@echo "Papers directory removed"

# Lint org files
lint-org:
	@echo "Linting all org files..."
	@REPO_ROOT=$(shell pwd) emacs --batch --script scripts/lint-org.el

##
## Setup and Dependency Checking
##

setup: check-deps resources
	@echo "✓ Setup complete"

check-deps: check-racket check-emacs check-direnv
	@echo "✓ All dependencies checked"

check-racket:
	@echo "Checking Racket..."
	@which racket >/dev/null 2>&1 || (echo "✗ Racket not found. Install: sudo pkg install racket-minimal" && exit 1)
	@which raco >/dev/null 2>&1 || (echo "✗ raco not found" && exit 1)
	@echo "✓ Racket: $$(racket --version)"

check-emacs:
	@echo "Checking Emacs..."
	@which emacs >/dev/null 2>&1 || (echo "✗ Emacs not found. Install: sudo pkg install emacs" && exit 1)
	@emacs --version | head -1 | grep -qE '(30\.|[3-9][0-9]\.)' || (echo "✗ Emacs 30+ required, found: $$(emacs --version | head -1)" && exit 1)
	@echo "✓ Emacs: $$(emacs --version | head -1)"

check-direnv:
	@which direnv >/dev/null 2>&1 && echo "✓ direnv available" || echo "ℹ direnv not installed (optional)"

##
## Testing
##

test: check-deps
	@echo "Running tests..."
	@./test/run-tests.sh
