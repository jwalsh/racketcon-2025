#!/bin/bash
# Quick Start Guide for Running All Simulations

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║  Distributed Callback Problem - Run All Simulations      ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

# Check if racket is installed
if ! command -v racket &> /dev/null; then
    echo "❌ Racket not found. Please install:"
    echo "   brew install racket  # macOS"
    echo "   sudo apt-get install racket  # Linux"
    exit 1
fi

# Check if roulette is installed
if ! racket -e '(require roulette/example/disrupt)' &> /dev/null; then
    echo "❌ Roulette package not found. Installing..."
    raco pkg install roulette
fi

echo "✅ Prerequisites OK"
echo ""

# Run each simulation
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "1. BLUE/GREEN DEPLOYMENT (50/50)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
racket simulation/callback-simulation.rkt
echo ""
read -p "Press Enter to continue..."
echo ""

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "2. STABLE/CANARY DEPLOYMENT (90/10)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
racket simulation/stable-canary-simulation.rkt
echo ""
read -p "Press Enter to continue..."
echo ""

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "3. LOAD BALANCER (10 SERVERS)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
racket simulation/ten-server-simulation.rkt
echo ""
read -p "Press Enter to continue..."
echo ""

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "4. N-SERVER ANALYSIS (2 to 100 servers)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
racket simulation/n-server-analysis.rkt
echo ""
read -p "Press Enter to continue..."
echo ""

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "5. COMPREHENSIVE COMPARISON"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
racket simulation/comparison-all-scenarios.rkt
echo ""

echo "═══════════════════════════════════════════════════════════"
echo "All simulations complete!"
echo "═══════════════════════════════════════════════════════════"
