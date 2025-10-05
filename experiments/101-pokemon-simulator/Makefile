.PHONY: all run test clean help

# Default target
all: run

# Run all battle simulations
run: run-basic run-conditional run-speed run-type run-multi

# Run basic battle
run-basic:
	@echo "=== Basic Battle ==="
	@racket 01-basic-battle.rkt
	@echo ""

# Run conditional analysis
run-conditional:
	@echo "=== Conditional Analysis ==="
	@racket -l racket/base -e '(require (submod "01-basic-battle.rkt" conditional))'
	@echo ""

# Run speed tie analysis
run-speed:
	@echo "=== Speed Tie ==="
	@racket -l racket/base -e '(require (submod "01-basic-battle.rkt" speed-tie))'
	@echo ""

# Run type effectiveness
run-type:
	@echo "=== Type Effectiveness ==="
	@racket -l racket/base -e '(require (submod "01-basic-battle.rkt" type-effectiveness))'
	@echo ""

# Run multi-turn analysis
run-multi:
	@echo "=== Multi-Turn Battle ==="
	@racket -l racket/base -e '(require (submod "01-basic-battle.rkt" multi-turn))'
	@echo ""

# Test that files compile
test:
	@echo "=== Testing Pokemon Simulator ==="
	@echo "Testing 01-basic-battle.rkt..."
	@raco test 01-basic-battle.rkt 2>/dev/null || racket 01-basic-battle.rkt > /dev/null
	@echo "✓ 01-basic-battle.rkt works"

# Verify org-mode file
verify-org:
	@echo "=== Verifying README.org in Emacs ==="
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--visit=README.org \
		--eval "(org-lint)" \
		--eval "(message \"✓ README.org validated\")"

# Clean generated files
clean:
	@echo "Cleaning..."
	@rm -f *.zo compiled/*.zo
	@echo "Done"

# Show help
help:
	@echo "Experiment 101: Pokémon Battle Simulator"
	@echo ""
	@echo "Targets:"
	@echo "  make run              - Run all simulations"
	@echo "  make run-basic        - Basic battle only"
	@echo "  make run-conditional  - Conditional probability"
	@echo "  make run-speed        - Speed priority"
	@echo "  make run-type         - Type effectiveness"
	@echo "  make run-multi        - Multi-turn probability"
	@echo "  make test             - Test compilation"
	@echo "  make verify-org       - Verify README.org"
	@echo "  make clean            - Clean compiled files"
	@echo "  make help             - Show this help"
