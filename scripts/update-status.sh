#!/bin/bash
# Update experiment status from GitHub

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "Fetching GitHub issues..."
gh issue list --json number,title,state,labels,assignees --limit 100 > /tmp/issues.json

echo "Fetching GitHub PRs..."
gh pr list --json number,title,state,labels --limit 100 > /tmp/prs.json

echo "Generating org-mode report..."
racket "$SCRIPT_DIR/process-issues.rkt" /tmp/issues.json > "$REPO_ROOT/EXPERIMENT_STATUS.org"

echo "✓ Status report updated: EXPERIMENT_STATUS.org"
echo ""
echo "Summary:"
echo "--------"
racket -e "(require json) \
  (define data (call-with-input-file \"/tmp/issues.json\" read-json)) \
  (printf \"Total Issues: ~a\n\" (length data)) \
  (printf \"Open: ~a\n\" (length (filter (λ (i) (equal? (hash-ref i 'state) \"OPEN\")) data))) \
  (printf \"Closed: ~a\n\" (length (filter (λ (i) (equal? (hash-ref i 'state) \"CLOSED\")) data)))"
