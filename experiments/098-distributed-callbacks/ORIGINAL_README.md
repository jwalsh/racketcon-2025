# Distributed Callback Problem Analysis

This repository demonstrates the callback routing problem in load-balanced systems with server-local state.

## Problem Summary

When a load-balanced application:
1. Stores state locally (per-server Redis instances)
2. Makes external API calls with callbacks to a load-balanced endpoint
3. The callback has a 50% chance of hitting the wrong server

**Result: 50% failure rate** due to independent routing decisions.

## Files

### Documentation
- `distributed-callback-problem.org` - Main analysis document (org-mode with Babel)
- `README.md` - This file

### Simulations (All using Roulette)

#### Basic 50/50 Scenarios
- `simulation/callback-simulation.rkt` - Basic 2-server simulation
- `simulation/callback-simulation-detailed.rkt` - Detailed with all 4 scenarios

#### Extended Scenarios
- `simulation/stable-canary-simulation.rkt` - **90/10 split (18% failure rate)**
- `simulation/ten-server-simulation.rkt` - **10 servers (90% failure rate)**
- `simulation/n-server-analysis.rkt` - Generalized N-server analysis
- `simulation/comparison-all-scenarios.rkt` - Side-by-side comparison

## Running the Simulations

### Prerequisites

Install Racket and the roulette library:

```bash
# Install Racket (if not already installed)
# On Ubuntu/Debian:
sudo apt-get install racket

# On macOS:
brew install racket

# Install roulette package
raco pkg install roulette
```

### Run Simulations

#### 1. Basic Blue/Green (50/50)

```bash
racket simulation/callback-simulation.rkt
```

Expected: **50% success, 50% failure**

#### 2. Stable/Canary (90/10)

```bash
racket simulation/stable-canary-simulation.rkt
```

Expected output shows:
- Both Stable: 81%
- Both Canary: 1%
- **Overall Success: 82%, Failure: 18%**

#### 3. Load Balancer (10 Servers)

```bash
racket simulation/ten-server-simulation.rkt
```

Expected: **10% success, 90% failure** 😱

#### 4. N-Server Analysis

```bash
racket simulation/n-server-analysis.rkt
```

Shows how failure rate scales from 2 to 100 servers.

#### 5. Comprehensive Comparison

```bash
racket simulation/comparison-all-scenarios.rkt
```

Runs all scenarios side-by-side with PMF distributions.

## Generating Diagrams from Org-Mode

If you have Emacs with org-mode and mermaid support:

```bash
emacs distributed-callback-problem.org
```

Then execute the mermaid blocks with `C-c C-c` to generate diagrams in the `diagrams/` directory.

Alternatively, use the Mermaid CLI:

```bash
# Install mermaid-cli
npm install -g @mermaid-js/mermaid-cli

# Generate diagrams (if extracted from org-mode)
mmdc -i architecture-flow.mmd -o diagrams/architecture-flow.png
```

## Key Insights

### The Problem Scales with Server Count

| Deployment Type | Servers | Split     | Success | Failure | Use Case           |
|-----------------|---------|-----------|---------|---------|-------------------|
| Blue/Green      | 2       | 50/50     | 50%     | 50%     | A/B Testing       |
| Stable/Canary   | 2       | 90/10     | 82%     | **18%** | Gradual Rollouts  |
| Load Balanced   | 10      | 10% each  | 10%     | **90%** | Production Scale  |
| Large Cluster   | 100     | 1% each   | 1%      | **99%** | Enterprise Scale  |

### Formula: Failure Rate = (N-1)/N

As you add servers:
- 2 servers → 50% failure
- 10 servers → 90% failure  
- 100 servers → 99% failure
- As N → ∞, failure → 100%

### The Paradox

**Horizontal scaling makes this problem WORSE**, not better!

### Roulette Demonstrates This Probabilistically

1. **Two Independent Events**: Initial routing and callback routing
2. **Success = Same Server**: `P(success) = 1/N`
3. **Failure = Different Server**: `P(failure) = (N-1)/N`

## Solutions

See the org-mode document for detailed solutions including:
- Centralized/shared Redis
- Sticky sessions with server hints
- Consistent hashing
- Message queue architecture
- State replication

## License

Public domain / CC0
