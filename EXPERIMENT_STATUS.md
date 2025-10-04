# RacketCon 2025 - Experiment Status

## Overview

Comprehensive tracking of all experiments with testing infrastructure and GitHub integration.

## Completed Experiments with Makefiles

### Track 1: Formal Verification & Rosette

| ID | Experiment | Status | Makefile | Tests | GitHub Issue |
|----|------------|--------|----------|-------|--------------|
| 000 | Racket & Rosette Setup | ✅ CLOSED | ✅ | Setup/Verify | [#1](https://github.com/jwalsh/racketcon-2025/issues/1) |
| 001 | Rosette Fundamentals | ✅ CLOSED | ✅ | 4 examples | [#2](https://github.com/jwalsh/racketcon-2025/issues/2) |
| 002 | Formal Methods Overview | ✅ CLOSED | - | Docs only | [#3](https://github.com/jwalsh/racketcon-2025/issues/3) |
| 003 | Rosette vs Lean vs TLA+ | ✅ CLOSED | - | Docs only | [#8](https://github.com/jwalsh/racketcon-2025/issues/8) |
| 004 | DNS Tools (macOS/FreeBSD) | ✅ CLOSED | - | Docs only | [#9](https://github.com/jwalsh/racketcon-2025/issues/9) |
| 005 | Cloudflare DNS Setup | ✅ CLOSED | - | Docs only | [#10](https://github.com/jwalsh/racketcon-2025/issues/10) |
| 006 | Topaz-lang Exploration | ✅ CLOSED | - | Docs only | [#11](https://github.com/jwalsh/racketcon-2025/issues/11) |
| 007 | Compositional OOP | ✅ CLOSED | - | Docs only | [#12](https://github.com/jwalsh/racketcon-2025/issues/12) |
| 008 | Rosette String Limitations | ✅ CLOSED | - | Docs only | [#13](https://github.com/jwalsh/racketcon-2025/issues/13) |

### Track 3: Pattern Matching & DSLs

| ID | Experiment | Status | Makefile | Tests | GitHub Issue |
|----|------------|--------|----------|-------|--------------|
| 041 | Deep Immutable Updates | 🔄 IN_PROGRESS | - | - | [#15](https://github.com/jwalsh/racketcon-2025/issues/15) (@aygp-dr) |
| 043 | Lens-based Updates | 🔄 IN_PROGRESS | - | - | [#16](https://github.com/jwalsh/racketcon-2025/issues/16) (@dsp-dr) |
| 045 | Functional Lenses | ✅ CLOSED | ✅ | Lens laws | [#4](https://github.com/jwalsh/racketcon-2025/issues/4) |
| 046 | Pattern Matching | ✅ CLOSED | ✅ | Patterns | [#5](https://github.com/jwalsh/racketcon-2025/issues/5) |
| 047 | Ocular Patdown | ✅ CLOSED | ✅ | Lens/Prism/Traversal | [#6](https://github.com/jwalsh/racketcon-2025/issues/6) |
| 048 | Ruby Lenses | ✅ CLOSED | ✅ | Lens/Traversal | [#7](https://github.com/jwalsh/racketcon-2025/issues/7) |

### Track 4: Data Formats

| ID | Experiment | Status | Makefile | Tests | GitHub Issue |
|----|------------|--------|----------|-------|--------------|
| 061 | Ion Format Basics | ✅ CLOSED | ✅ | Docs/Examples | [#14](https://github.com/jwalsh/racketcon-2025/issues/14) |

### Track 5: Editor Integration

| ID | Experiment | Status | Makefile | Tests | GitHub Issue |
|----|------------|--------|----------|-------|--------------|
| 076 | Geiser Fundamentals | ✅ CLOSED | ✅ | Guile/Emacs | [#18](https://github.com/jwalsh/racketcon-2025/issues/18) |
| 077 | Geiser with Racket | ✅ CLOSED | - | Comparison | [#19](https://github.com/jwalsh/racketcon-2025/issues/19) |

### Framework

| ID | Experiment | Status | Makefile | Tests | GitHub Issue |
|----|------------|--------|----------|-------|--------------|
| 999 | RHEA Framework | ✅ CLOSED | ✅ | 3 langs | [#17](https://github.com/jwalsh/racketcon-2025/issues/17) |

## Makefile Summary

### Experiments with Make Targets

All Makefiles include standard targets:
- `make help` - Show available commands
- `make info` - Display experiment information
- `make test` - Run tests/examples
- `make clean` - Remove generated files

#### Special Targets by Experiment

**000-racket-setup**:
```bash
make setup    # Install Rosette
make test     # Verify installation
```

**001-rosette-fundamentals**:
```bash
make check-rosette    # Verify Rosette installed
make test             # Run symbolic, verify, synthesize, debug
```

**045-functional-lenses**:
```bash
make test        # Run lens laws verification
make quick       # Fast lens law check
make verify-laws # Comprehensive verification
make benchmark   # Performance testing
make repl        # Interactive REPL with lenses
```

**046-pattern-matching**:
```bash
make test   # Run pattern matching examples
make quick  # Run comprehensive patterns
```

**047-ocular-patdown**:
```bash
make test-lenses      # Lens examples
make test-prisms      # Prism examples
make test-traversals  # Traversal examples
make install-haskell  # Install Haskell for comparison
```

**048-ruby-lenses**:
```bash
make test-lens      # Ruby lens tests
make test-traversal # Traversal tests
# Note: prism has syntax errors (Ruby 2.6 compatibility)
```

**061-ion-format-basics**:
```bash
make view   # View Ion examples
```

**076-geiser-fundamentals**:
```bash
make test    # Run Guile tests
make emacs   # Launch Emacs with Geiser
make install # Install Geiser packages
```

**999-rhea-framework**:
```bash
# Racket version
cd experiments/999-rhea-framework
racket rhea.rkt

# Python version
python3 rhea.py

# Guile version (if guile3 available)
guile rhea.scm
```

## Getting Started

### Quick Test All Experiments

```bash
# Test experiments with Makefiles
for exp in 000 001 045 046 047 048 061 076; do
  cd experiments/${exp}-*
  make test 2>&1 | head -20
  cd ../..
done
```

### Test Specific Track

```bash
# Track 1: Rosette/Formal Methods
cd experiments/000-racket-setup && make setup && make test
cd experiments/001-rosette-fundamentals && make test

# Track 3: Lenses/Patterns
cd experiments/045-functional-lenses && make test
cd experiments/046-pattern-matching && make test
cd experiments/047-ocular-patdown && make test

# Track 5: Editor Integration
cd experiments/076-geiser-fundamentals && make test
```

## Statistics

### By Status
- ✅ Closed: 16 experiments
- 🔄 In Progress: 2 experiments (@aygp-dr, @dsp-dr)
- 📋 Total: 18 experiments documented

### By Type
- With Makefiles: 9 experiments
- Documentation only: 7 experiments
- Framework: 1 (RHEA)

### By Track
- Track 1 (Formal Methods): 9 experiments
- Track 3 (Lenses/Patterns): 6 experiments
- Track 4 (Data Formats): 1 experiment
- Track 5 (Editor Integration): 2 experiments

## RHEA Framework Integration

All experiments follow the RHEA (Racket Hypothesis-Experiment-Analysis) framework:

1. **Hypothesis**: Research question, assumptions, predictions
2. **Experiment**: Setup, procedure, data collection
3. **Analysis**: Results, validation, next steps

See: [experiments/999-rhea-framework/](experiments/999-rhea-framework/)

## Recent Commits

```
624e3a5 feat(experiments): add Makefiles for experiments 000, 001, 048, 061
0cdff58 feat(experiments): add Makefiles and Geiser experiments
c7bf925 docs(045-lenses): add getting started guide
410eb98 feat(045-lenses): add Makefile for testing and verification
dfcc20b docs(rhea): add experiment assignment tracking
64a5d9d feat(rhea): implement RHEA framework in Racket/Guile/Python
```

## Next Steps

### For Assignees (@aygp-dr, @dsp-dr)

1. Review RHEA framework: [experiments/999-rhea-framework/README.org](experiments/999-rhea-framework/README.org)
2. Complete assigned experiments (#15, #16)
3. Add Makefiles to experiments
4. Update GitHub issues with findings

### Future Experiments

Planned experiments from EXPERIMENTS.org:
- Track 2: OOP (021-040)
- Track 6: Probabilistic Programming (086-095)
- Track 7: Logic Programming (096-110)
- Track 8: Network Protocols (111-125)
- And more... (310 total planned)

## Resources

- [RHEA Framework Summary](experiments/999-rhea-framework/SUMMARY.org)
- [Experiment Assignments](experiments/999-rhea-framework/EXPERIMENT_STATUS.md)
- [GitHub Issues](https://github.com/jwalsh/racketcon-2025/issues)
- [RacketCon 2025 Sessions](sessions.org)
