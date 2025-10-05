;;; emacs-config.el --- Complete Emacs + Racket Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon

;; This file provides a complete Emacs configuration for Racket development
;; combining racket-mode, geiser, paredit, and org-babel.

;;; Commentary:

;; Complete setup for Racket development in Emacs including:
;; - Racket Mode (primary)
;; - Geiser (alternative/complement)
;; - Paredit for structural editing
;; - Rainbow Delimiters for visual clarity
;; - Company for auto-completion
;; - Org-Babel integration for literate programming

;;; Code:

;; ============================================================================
;; Package Management
;; ============================================================================

(require 'package)

;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Refresh package list if needed
(unless package-archive-contents
  (package-refresh-contents))

;; ============================================================================
;; Install Required Packages
;; ============================================================================

(defvar racket-emacs-packages
  '(racket-mode
    geiser
    geiser-racket
    paredit
    smartparens
    rainbow-delimiters
    company
    which-key)
  "List of packages required for Racket development.")

(dolist (pkg racket-emacs-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; ============================================================================
;; Racket Mode Configuration
;; ============================================================================

(require 'racket-mode)

;; Set Racket program paths (adjust for your system)
(setq racket-program
      (or (executable-find "racket")
          "/usr/local/bin/racket"))

(setq racket-racket-program racket-program)

(setq racket-raco-program
      (or (executable-find "raco")
          "/usr/local/bin/raco"))

;; Auto-mode for Racket files
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.rktd\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.rktl\\'" . racket-mode))

;; REPL configuration
(setq racket-repl-buffer-name-function #'racket-repl-buffer-name-project)
(setq racket-show-functions '(racket-show-echo-area))

;; Indentation
(setq racket-indent-curly-as-sequence t)
(setq racket-indent-sequence-depth 'always)

;; Documentation
(setq racket-documentation-search-location 'local) ; or 'remote

;; Hooks
(add-hook 'racket-mode-hook #'racket-xp-mode) ; Cross-reference mode

;; ============================================================================
;; Geiser Configuration
;; ============================================================================

(require 'geiser)
(require 'geiser-racket)

(setq geiser-active-implementations '(racket))
(setq geiser-racket-binary racket-program)
(setq geiser-racket-use-gracket-p nil)

;; Geiser REPL
(setq geiser-repl-startup-time 10000) ; 10 seconds timeout
(setq geiser-repl-history-filename
      (expand-file-name "geiser-history" user-emacs-directory))

;; Auto-completion
(setq geiser-completion-case-sensitive-p nil)

;; ============================================================================
;; Paredit - Structural Editing
;; ============================================================================

(require 'paredit)

;; Enable in Racket buffers
(add-hook 'racket-mode-hook #'enable-paredit-mode)
(add-hook 'racket-repl-mode-hook #'enable-paredit-mode)

;; Enable in scheme-mode (for Geiser)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)

;; Show matching parens
(add-hook 'paredit-mode-hook #'show-paren-mode)

;; ============================================================================
;; Rainbow Delimiters
;; ============================================================================

(require 'rainbow-delimiters)

(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode)

;; ============================================================================
;; Company - Auto-completion
;; ============================================================================

(require 'company)

(add-hook 'racket-mode-hook #'company-mode)
(add-hook 'racket-repl-mode-hook #'company-mode)

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)

;; ============================================================================
;; Which-Key - Keybinding Help
;; ============================================================================

(require 'which-key)
(which-key-mode 1)

;; ============================================================================
;; Org-Babel Integration
;; ============================================================================

(require 'org)
(require 'ob)

;; Try to load ob-racket if available
(condition-case nil
    (require 'ob-racket)
  (error
   ;; Fallback to ob-scheme with Racket
   (require 'ob-scheme nil t)
   (setq org-babel-scheme-cmd racket-program)))

;; Enable Racket in org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . t)
   (scheme . t)
   (emacs-lisp . t)))

;; Don't ask for confirmation every time
(setq org-confirm-babel-evaluate nil)

;; Syntax highlighting in code blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; ============================================================================
;; Custom Key Bindings
;; ============================================================================

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "C-c C-d") 'racket-describe)
  (define-key racket-mode-map (kbd "C-c C-c") 'racket-run)
  (define-key racket-mode-map (kbd "C-c C-k") 'racket-run-and-switch-to-repl)
  (define-key racket-mode-map (kbd "C-c C-z") 'racket-repl)
  (define-key racket-mode-map (kbd "C-c C-e") 'racket-expand-last-sexp)
  (define-key racket-mode-map (kbd "C-c C-t") 'racket-test)
  (define-key racket-mode-map (kbd "C-c C-l") 'racket-logger)
  (define-key racket-mode-map (kbd "C-M-.") 'racket-visit-definition)
  (define-key racket-mode-map (kbd "C-M-,") 'racket-unvisit))

;; ============================================================================
;; UI Enhancements
;; ============================================================================

;; Line numbers (Emacs 26+)
(when (fboundp 'display-line-numbers-mode)
  (add-hook 'racket-mode-hook #'display-line-numbers-mode))

;; Highlight current line
(add-hook 'racket-mode-hook #'hl-line-mode)

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defun racket-send-sexp-to-repl ()
  "Send the sexp at point to the Racket REPL."
  (interactive)
  (racket-send-region (point) (save-excursion (forward-sexp) (point))))

(defun racket-eval-buffer-and-switch ()
  "Evaluate entire buffer and switch to REPL."
  (interactive)
  (racket-run)
  (racket-repl))

(defun racket-reload-module ()
  "Reload the current module in REPL."
  (interactive)
  (save-buffer)
  (racket-run))

;; ============================================================================
;; Project-Specific Settings
;; ============================================================================

(defun racket-project-setup ()
  "Setup for Racket projects."
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil))

(add-hook 'racket-mode-hook #'racket-project-setup)

;; ============================================================================
;; Messages
;; ============================================================================

(message "Racket Emacs Configuration Loaded")
(message "  - Racket Mode: %s" (if (featurep 'racket-mode) "✓" "✗"))
(message "  - Geiser: %s" (if (featurep 'geiser) "✓" "✗"))
(message "  - Paredit: %s" (if (featurep 'paredit) "✓" "✗"))
(message "  - Rainbow Delimiters: %s" (if (featurep 'rainbow-delimiters) "✓" "✗"))
(message "  - Company: %s" (if (featurep 'company) "✓" "✗"))

(provide 'emacs-config)
;;; emacs-config.el ends here
