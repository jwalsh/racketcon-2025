# Makefile for RacketCon 2025 experiments
# Use gmake (GNU Make) not BSD make

.PHONY: help download-papers clean-papers rosette-paper lint-org

# Default target
help:
	@echo "RacketCon 2025 - Available targets:"
	@echo ""
	@echo "  download-papers - Download all research papers"
	@echo "  rosette-paper   - Download Rosette PLDI 2014 paper"
	@echo "  clean-papers    - Remove downloaded papers"
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
	curl -L -o $(ROSETTE_PAPER) $(ROSETTE_URL)
	@echo "Downloaded: $(ROSETTE_PAPER)"

rosette-paper: $(ROSETTE_PAPER)

# Additional papers can be added here
# Example:
# TRUFFLE_PAPER := $(PAPERS_DIR)/truffle-paper.pdf
# TRUFFLE_URL := https://example.com/truffle.pdf
#
# $(TRUFFLE_PAPER): $(PAPERS_DIR)
# 	curl -L -o $(TRUFFLE_PAPER) $(TRUFFLE_URL)

# Download all papers
download-papers: rosette-paper
	@echo "All papers downloaded to $(PAPERS_DIR)/"

# Clean papers directory
clean-papers:
	rm -rf $(PAPERS_DIR)
	@echo "Papers directory removed"

# Lint org files
lint-org:
	@echo "Linting all org files..."
	@REPO_ROOT=$(shell pwd) emacs --batch --script scripts/lint-org.el
