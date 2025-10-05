;;; install-packages.el --- Install required packages for RacketCon 2025 -*- lexical-binding: t; -*-

;;; Commentary:
;; Run this file to install all required packages for RacketCon 2025
;; Usage: emacs --batch -l install-packages.el

;;; Code:

;; Initialize package.el
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Refresh package contents
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; List of required packages
(defvar racketcon-required-packages
  '(;; Core Racket support
    racket-mode
    geiser
    geiser-racket
    geiser-guile

    ;; Structural editing
    paredit
    smartparens
    rainbow-delimiters

    ;; Completion
    company

    ;; Project management
    projectile

    ;; Git integration
    magit

    ;; Syntax checking
    flycheck

    ;; Discoverability
    which-key

    ;; Optional but useful
    helm
    counsel
    ivy

    ;; Org mode enhancements
    org
    ob-racket)
  "Required packages for RacketCon 2025.")

;; Install all packages
(message "Installing RacketCon 2025 packages...")
(dolist (package racketcon-required-packages)
  (condition-case err
      (progn
        (unless (package-installed-p package)
          (message "Installing %s..." package)
          (package-install package))
        (message "✓ %s installed" package))
    (error
     (message "✗ Failed to install %s: %s" package (error-message-string err)))))

(message "Package installation complete!")

;;; install-packages.el ends here
