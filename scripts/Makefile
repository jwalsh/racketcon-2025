# Makefile for experiment utilities

.PHONY: all update-status help clean

all: update-status

help:
	@echo "Experiment Status Utilities"
	@echo ""
	@echo "  make update-status  - Fetch issues and update EXPERIMENT_STATUS.org"
	@echo "  make clean          - Remove temp files"

update-status:
	@./update-status.sh

clean:
	@rm -f /tmp/issues.json /tmp/prs.json
	@echo "✓ Clean complete"
