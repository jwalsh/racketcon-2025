# Distributed Callback Problem - Visual Failure Rates

## The Escalating Disaster

```
                    CALLBACK FAILURE RATE BY SERVER COUNT
                                
    100% ┤                                              ████████
         │                                         █████████
         │                                    █████████
         │                               █████████
     90% ┤                          █████████              ← 10 servers
         │                     █████████
         │                █████████
         │           █████████
     80% ┤      █████████
         │ █████████
         │████
         │
     70% ┤
         │
         │
         │
     60% ┤
         │
         │
         │
     50% ┤██████                                         ← 2 servers (50/50)
         │
         │
         │
     40% ┤
         │
         │
         │
     30% ┤
         │
         │
         │
     20% ┤
         │█████                                           ← 2 servers (90/10)
     10% ┤
         │
      0% ┼─────────────────────────────────────────────────────────────
         1    2     5    10    20    30    40    50    60    70   100
                        NUMBER OF SERVERS
```

## The Math

```
Deployment          Servers    Formula        Success    Failure
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Blue/Green (50/50)     2       1/2             50%        50% ██████
Stable/Canary (90/10)  2       0.9²+0.1²       82%        18% ██
Load Balancer          10      1/10            10%        90% ███████████
Large Cluster          100     1/100            1%        99% ████████████
Infinite Servers       ∞       1/∞              0%       100% █████████████
```

## Roulette PMF Representation

### 2 Servers (50/50)
```
Initial:  (pmf | #t ↦ 0.5 | #f ↦ 0.5)
Callback: (pmf | #t ↦ 0.5 | #f ↦ 0.5)
Success:  (pmf | #t ↦ 0.5 | #f ↦ 0.5)  ← 50% chance
```

### 2 Servers (90/10)
```
Initial:  (pmf | #t ↦ 0.9 | #f ↦ 0.1)
Callback: (pmf | #t ↦ 0.9 | #f ↦ 0.1)
Success:  (pmf | #t ↦ 0.82 | #f ↦ 0.18)  ← 82% chance
```

### 10 Servers
```
Initial:  (pmf | 0 ↦ 0.1 | 1 ↦ 0.1 | ... | 9 ↦ 0.1)
Callback: (pmf | 0 ↦ 0.1 | 1 ↦ 0.1 | ... | 9 ↦ 0.1)
Success:  (pmf | #t ↦ 0.1 | #f ↦ 0.9)  ← Only 10% chance!
```

## The Counterintuitive Reality

```
More Servers = Higher Availability ✓
More Servers = Better Performance ✓
More Servers = More Callback Failures ✗

This is why you need:
  • Shared state (centralized Redis, DB)
  • Sticky sessions (session affinity)
  • Message queues (decouple callback)
  • Consistent hashing
```

## Real-World Impact

If you handle 1000 requests/minute:

| Servers | Success | Failures/min | Lost Revenue (@ $10/order) |
|---------|---------|--------------|---------------------------|
| 2 (50/50) | 50% | 500 | $5,000/min |
| 2 (90/10) | 82% | 180 | $1,800/min |
| 10 | 10% | **900** | **$9,000/min** |
| 100 | 1% | **990** | **$9,900/min** |

**At scale, this bug costs millions annually.**
