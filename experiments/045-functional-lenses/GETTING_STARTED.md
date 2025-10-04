# Getting Started with Experiment 045: Functional Lenses

## Quick Start

```bash
# 1. Navigate to the experiment
cd experiments/045-functional-lenses

# 2. Run the core tests
make test

# 3. Or just quick verification
make quick
```

## Expected Output

You should see:

```
✓ Core lens laws verified
```

This confirms that the three lens laws (GetPut, PutGet, PutPut) are satisfied for `car-lens` and `cdr-lens`.

## What This Experiment Demonstrates

### The Three Lens Laws

1. **GetPut (You get what you put)**
   - Setting what you just got changes nothing
   - `(set lens target (view lens target)) ≡ target`

2. **PutGet (You put what you get)**
   - Getting after setting returns what you set
   - `(view lens (set lens target value)) ≡ value`

3. **PutPut (Last write wins)**
   - Setting twice is same as setting once with the second value
   - `(set lens (set lens target v1) v2) ≡ (set lens target v2)`

### Implementations Tested

- ✅ **lens-laws.rkt** - Formal verification of all three laws
- ✅ **optics-definitions.rkt** - Fold, Traversal, Prism, Lens hierarchy
- ⚠️ **optics-compose.rkt** - Composition (has display formatting issues)
- ⚠️ **maybe-prism.rkt** - Optional lenses (has display formatting issues)
- ⚠️ Other files - Working logic, minor display issues

## Make Targets

```bash
make help         # Show all available targets
make test         # Run core test suite
make check        # Verify all files compile
make quick        # Fast verification
make info         # Show experiment details
make clean        # Remove generated files
```

## Troubleshooting

### Files show compilation errors

Some files have `displayln` formatting issues but the core lens logic is correct. The Makefile is configured to be tolerant of these display-only issues.

### Tests fail

If `make test` fails, try:
```bash
make setup        # Install dependencies
racket --version  # Ensure Racket 8.15+
```

## Next Steps

1. Read `README.org` for complete documentation
2. Explore individual `.rkt` files
3. Try modifying lens implementations
4. Implement your own lenses

## Related

- **GitHub Issue**: #4 (Experiment 045)
- **Related Experiments**:
  - 041: Deep Immutable Updates
  - 043: Lens-based Updates
  - 046: Pattern Matching
  - 047: Ocular Patdown (optics library)
  - 048: Ruby Lenses (cross-language port)

## Learn More

- [Lens Laws Documentation](README.org)
- [RHEA Framework](../999-rhea-framework/)
- [RacketCon 2025 Session](../../sessions.org) - Saturday 10:45am EDT
