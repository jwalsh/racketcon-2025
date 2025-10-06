#!/bin/sh
# Comprehensive tool check for RacketCon 2025 experiments
# Generated during 2025-10-05 audit

echo "=== Core Build Tools ==="
check_tool() {
    if command -v "$1" >/dev/null 2>&1; then
        version=$("$1" $2 2>&1 | head -1)
        echo "✓ $1: $version"
        return 0
    else
        echo "✗ $1: NOT FOUND"
        return 1
    fi
}

# GNU Make (critical)
check_tool "gmake" "--version"

# Git tools
check_tool "git" "--version"
check_tool "gh" "--version"
check_tool "ghq" "--version"

echo ""
echo "=== Racket Ecosystem ==="
check_tool "racket" "--version"
check_tool "raco" "version"

echo ""
echo "=== Emacs ==="
if command -v emacs >/dev/null 2>&1; then
    emacs_ver=$(emacs --version | head -1)
    echo "✓ emacs: $emacs_ver"
    if echo "$emacs_ver" | grep -qE '(30\.|[3-9][0-9]\.)'; then
        echo "  ✓ Version 30+ requirement met"
    else
        echo "  ✗ Version 30+ required, found: $emacs_ver"
    fi
else
    echo "✗ emacs: NOT FOUND"
fi

echo ""
echo "=== Python Tools ==="
check_tool "python3" "--version"
check_tool "uv" "--version"

echo ""
echo "=== Additional Languages ==="
check_tool "ruby" "--version"
check_tool "guile" "--version"

echo ""
echo "=== Download Tools ==="
if command -v curl >/dev/null 2>&1; then
    check_tool "curl" "--version"
elif command -v fetch >/dev/null 2>&1; then
    check_tool "fetch" "--version"
else
    echo "✗ curl or fetch: NOT FOUND (needed for downloading papers)"
fi

echo ""
echo "=== Optional Tools ==="
if command -v direnv >/dev/null 2>&1; then
    check_tool "direnv" "version"
else
    echo "ℹ direnv: not installed (optional)"
fi

echo ""
echo "=== Racket Packages ==="
if command -v raco >/dev/null 2>&1; then
    echo "Checking Racket packages..."
    if raco pkg show rosette >/dev/null 2>&1; then
        echo "✓ rosette: installed"
    else
        echo "✗ rosette: NOT INSTALLED (run: raco pkg install rosette)"
    fi
    
    if raco pkg show roulette >/dev/null 2>&1; then
        echo "✓ roulette: installed"
    else
        echo "✗ roulette: NOT INSTALLED (run: raco pkg install roulette)"
    fi
    
    if raco pkg show ocular-patdown >/dev/null 2>&1; then
        echo "✓ ocular-patdown: installed"
    else
        echo "ℹ ocular-patdown: not installed (needed for lens experiments)"
    fi
fi

echo ""
echo "=== Python Packages (uv) ==="
if command -v uv >/dev/null 2>&1; then
    if uv pip list 2>/dev/null | grep -q yt-dlp; then
        echo "✓ yt-dlp: installed"
    else
        echo "ℹ yt-dlp: not installed (run: uv pip install yt-dlp)"
    fi
fi
