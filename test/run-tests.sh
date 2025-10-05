#!/bin/sh
# run-tests.sh - Run RacketCon 2025 ERT tests

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "${YELLOW}=== RacketCon 2025 Test Runner ===${NC}"
echo ""

# Find project root
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
echo "Project root: $PROJECT_ROOT"
echo ""

# Change to project root
cd "$PROJECT_ROOT"

# Check if Emacs is available
if ! command -v emacs >/dev/null 2>&1; then
    echo "${RED}Error: emacs not found in PATH${NC}"
    exit 1
fi

# Check Emacs version
EMACS_VERSION=$(emacs --version | head -1)
echo "Emacs: $EMACS_VERSION"
echo ""

# Run tests
echo "${YELLOW}Running tests...${NC}"
echo ""

if emacs --batch \
    -l racketcon-2025-config.el \
    -l test/racketcon-test.el \
    -f ert-run-tests-batch-and-exit 2>&1 | tee /tmp/racketcon-test-output.log; then
    echo ""
    echo "${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    EXIT_CODE=$?
    echo ""
    echo "${RED}✗ Some tests failed (exit code: $EXIT_CODE)${NC}"
    echo ""
    echo "See /tmp/racketcon-test-output.log for details"
    exit $EXIT_CODE
fi
