#!/usr/bin/env bash

# Install Haskell and lens library for macOS and FreeBSD
# Supports both operating systems

set -e

OS=$(uname -s)

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║         Haskell + lens Library Installation                 ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""
echo "Detected OS: $OS"
echo ""

install_ghcup() {
    echo "Installing GHCup (Haskell installer)..."
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

    # Source ghcup environment
    [ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
}

install_haskell_macos() {
    echo "═══ macOS Installation ═══"
    echo ""

    # Check for Homebrew
    if ! command -v brew &> /dev/null; then
        echo "Homebrew not found. Installing..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    # Install GHC and Cabal via Homebrew (optional, GHCup is preferred)
    # brew install ghc cabal-install

    # Install via GHCup (recommended)
    install_ghcup

    echo ""
    echo "Installing GHC, Cabal, and HLS..."
    ghcup install ghc recommended
    ghcup install cabal recommended
    ghcup install hls recommended

    ghcup set ghc recommended
}

install_haskell_freebsd() {
    echo "═══ FreeBSD Installation ═══"
    echo ""

    # Update pkg
    sudo pkg update

    # Install GHC and Cabal
    echo "Installing GHC and Cabal-install..."
    sudo pkg install -y ghc hs-cabal-install

    # Or use GHCup
    # install_ghcup
}

install_lens_library() {
    echo ""
    echo "═══ Installing Haskell lens Library ═══"
    echo ""

    # Update cabal package list
    cabal update

    # Install lens and related libraries
    echo "Installing lens..."
    cabal install lens

    echo "Installing lens-related libraries..."
    cabal install lens-aeson
    cabal install microlens
    cabal install microlens-platform

    echo ""
    echo "✓ Lens libraries installed"
}

verify_installation() {
    echo ""
    echo "═══ Verifying Installation ═══"
    echo ""

    if command -v ghc &> /dev/null; then
        echo "GHC version: $(ghc --version)"
    else
        echo "⚠ GHC not found in PATH"
    fi

    if command -v cabal &> /dev/null; then
        echo "Cabal version: $(cabal --version | head -1)"
    else
        echo "⚠ Cabal not found in PATH"
    fi

    echo ""
    echo "Installed Haskell packages:"
    ghc-pkg list lens 2>/dev/null || echo "  (lens not yet installed)"
    ghc-pkg list microlens 2>/dev/null || echo "  (microlens not yet installed)"
}

create_test_file() {
    echo ""
    echo "═══ Creating Test File ═══"
    echo ""

    cat > lens-test.hs <<'EOF'
-- Simple lens example
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Person = Person
  { _name :: String
  , _age  :: Int
  } deriving Show

makeLenses ''Person

main :: IO ()
main = do
  let alice = Person "Alice" 30

  putStrLn "Original:"
  print alice

  putStrLn "\nUsing lens to get name:"
  print $ alice ^. name

  putStrLn "\nUsing lens to set name:"
  print $ alice & name .~ "Bob"

  putStrLn "\nUsing lens to modify age:"
  print $ alice & age %~ (+1)
EOF

    echo "Created lens-test.hs"
    echo ""
    echo "To test:"
    echo "  ghc lens-test.hs && ./lens-test"
}

# Main installation
case "$OS" in
    Darwin)
        install_haskell_macos
        ;;
    FreeBSD)
        install_haskell_freebsd
        ;;
    *)
        echo "Unsupported OS: $OS"
        echo "This script supports macOS and FreeBSD"
        exit 1
        ;;
esac

install_lens_library
verify_installation
create_test_file

echo ""
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║                 Installation Complete!                       ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""
echo "Next steps:"
echo "  1. Restart your terminal or run: source ~/.ghcup/env"
echo "  2. Test with: ghc lens-test.hs && ./lens-test"
echo "  3. Install more lens packages: cabal install <package>"
echo ""
echo "Useful lens packages:"
echo "  • lens          - Main lens library"
echo "  • lens-aeson    - Lenses for JSON (aeson)"
echo "  • microlens     - Lightweight lens alternative"
echo "  • generic-lens  - Generic programming with lenses"
echo ""
