# Experiment 000: Racket and Rosette Setup

## Overview

Initial setup for exploring Racket and Rosette, inspired by the RacketCon 2025 keynote on Cloudflare's use of Racket and Rosette for DNS verification.

## Prerequisites

- Racket installed (https://racket-lang.org/)
- raco package manager

## Installation

### Install Racket

```bash
# macOS
brew install --cask racket

# Or download from https://download.racket-lang.org/
```

### Install Rosette

```bash
raco pkg install rosette
```

## Verify Installation

```racket
#lang racket
(require rosette)
(displayln "Rosette is installed!")
```

Save as `verify.rkt` and run:

```bash
racket verify.rkt
```

## Basic Rosette Example

Create `hello-rosette.rkt`:

```racket
#lang rosette

(require rosette/lib/synthax)

; Simple verification example
(define-symbolic x integer?)
(define-symbolic y integer?)

; Verify that x + y = y + x (commutativity)
(verify
 (assert (= (+ x y) (+ y x))))

(displayln "Commutativity verified!")
```

Run:

```bash
racket hello-rosette.rkt
```

## Next Steps

- Explore solver-aided programming concepts
- Experiment with symbolic execution
- Build simple DSLs using Racket's #lang feature
- Study Cloudflare's topaz-lang approach

## Resources

- [Rosette Guide](https://docs.racket-lang.org/rosette-guide/)
- [Rosette Paper](https://emina.github.io/rosette/)
- [Racket Documentation](https://docs.racket-lang.org/)
