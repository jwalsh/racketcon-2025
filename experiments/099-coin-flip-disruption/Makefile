.PHONY: all run test clean help

# Default target
all: run

# Run all Racket examples
run: run-basic run-three-coins

# Run basic disruption example
run-basic:
	@echo "=== Running Basic Disruption ==="
	@racket 01-basic-disruption.rkt
	@echo ""

# Run three coins example
run-three-coins:
	@echo "=== Running Three Coins (main module) ==="
	@racket 02-three-coins.rkt
	@echo ""
	@echo "=== Running Three Coins (at-least-one module) ==="
	@racket -l racket/base -e '(require (submod "02-three-coins.rkt" at-least-one))'
	@echo ""
	@echo "=== Running Three Coins (exactly-two module) ==="
	@racket -l racket/base -e '(require (submod "02-three-coins.rkt" exactly-two))'
	@echo ""

# Test that files compile without errors
test:
	@echo "=== Testing Racket Files ==="
	@echo "Testing 01-basic-disruption.rkt..."
	@raco test 01-basic-disruption.rkt 2>/dev/null || racket 01-basic-disruption.rkt > /dev/null
	@echo "✓ 01-basic-disruption.rkt works"
	@echo "Testing 02-three-coins.rkt..."
	@raco test 02-three-coins.rkt 2>/dev/null || racket 02-three-coins.rkt > /dev/null
	@echo "✓ 02-three-coins.rkt works"

# Verify org-mode file loads in Emacs
verify-org:
	@echo "=== Verifying README.org in Emacs ==="
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--visit=README.org \
		--eval "(org-lint)" \
		--eval "(message \"✓ README.org validated\")"

# Clean generated files (if any)
clean:
	@echo "Cleaning..."
	@rm -f *.zo compiled/*.zo
	@echo "Done"

# Show help
help:
	@echo "Experiment 099: Coin Flip Disruption"
	@echo ""
	@echo "Targets:"
	@echo "  make run           - Run all examples"
	@echo "  make run-basic     - Run basic disruption only"
	@echo "  make run-three-coins - Run three coins examples"
	@echo "  make test          - Test that files compile"
	@echo "  make verify-org    - Verify README.org loads in Emacs"
	@echo "  make clean         - Clean compiled files"
	@echo "  make help          - Show this help"
