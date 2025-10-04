;;; init-geiser.el --- Geiser configuration for Experiment 076 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jason Walsh

;; Author: Jason Walsh
;; Keywords: scheme, geiser, emacs
;; Package-Requires: ((emacs "27.1") (geiser "0.28") (geiser-guile "0.28"))

;;; Commentary:

;; Geiser configuration for Experiment 076: Geiser Fundamentals
;; This file demonstrates best practices for Geiser setup.

;;; Code:

;;; ============================================================================
;;; Package Setup
;;; ============================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure Geiser packages are installed
(unless (package-installed-p 'geiser)
  (package-refresh-contents)
  (package-install 'geiser))

(unless (package-installed-p 'geiser-guile)
  (package-install 'geiser-guile))

;;; ============================================================================
;;; Core Geiser Configuration
;;; ============================================================================

(require 'geiser)
(require 'geiser-guile)

;; Set Guile binary
(setq geiser-guile-binary "guile")  ; or "guile3" on FreeBSD

;; Enable smart tab completion
(setq geiser-mode-smart-tab-p t)

;; Show autodoc in echo area
(setq geiser-autodoc-delay 0.2)
(setq geiser-autodoc-procedure-arity-flag t)

;; Use a dedicated REPL buffer
(setq geiser-repl-use-other-window t)
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; Save REPL history
(setq geiser-repl-history-size 1000)
(setq geiser-repl-save-debugging-history-p t)

;;; ============================================================================
;;; Guile-Specific Configuration
;;; ============================================================================

;; Load path for Guile modules
(setq geiser-guile-load-path
      (list (expand-file-name "~/code/scheme")
            (expand-file-name "~/.local/share/guile/site/3.0")))

;; Enable warnings
(setq geiser-guile-warning-level 'high)

;; Debug mode
(setq geiser-guile-debug-show-bt-p t)

;; Startup forms (executed when REPL starts)
(setq geiser-repl-startup-forms
      '("(use-modules (ice-9 readline))"
        "(activate-readline)"
        "(use-modules (ice-9 pretty-print))"
        "(use-modules (srfi srfi-1))"))

;;; ============================================================================
;;; Completion Configuration
;;; ============================================================================

(when (package-installed-p 'company)
  (add-hook 'geiser-mode-hook 'company-mode)
  (add-hook 'geiser-repl-mode-hook 'company-mode)

  ;; Company-specific settings for Geiser
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

;;; ============================================================================
;;; Display Configuration
;;; ============================================================================

;; Enable rainbow delimiters for better paren matching
(when (package-installed-p 'rainbow-delimiters)
  (add-hook 'geiser-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode))

;; Enable paredit for structured editing
(when (package-installed-p 'paredit)
  (add-hook 'geiser-mode-hook 'paredit-mode)
  (add-hook 'geiser-repl-mode-hook 'paredit-mode))

;;; ============================================================================
;;; Custom Keybindings
;;; ============================================================================

(with-eval-after-load 'geiser-mode
  (define-key geiser-mode-map (kbd "C-c C-a") 'geiser-autodoc-show)
  (define-key geiser-mode-map (kbd "C-c C-d C-d") 'geiser-doc-symbol-at-point)
  (define-key geiser-mode-map (kbd "C-c C-d C-m") 'geiser-doc-module)
  (define-key geiser-mode-map (kbd "C-c C-z") 'geiser-mode-switch-to-repl))

(with-eval-after-load 'geiser-repl
  (define-key geiser-repl-mode-map (kbd "C-c C-q") 'geiser-repl-exit)
  (define-key geiser-repl-mode-map (kbd "C-c C-d C-d") 'geiser-doc-symbol-at-point))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun geiser-eval-and-switch ()
  "Eval buffer and switch to REPL."
  (interactive)
  (geiser-eval-buffer)
  (geiser-mode-switch-to-repl))

(defun geiser-reload-module ()
  "Reload current module in REPL."
  (interactive)
  (geiser-eval-buffer)
  (message "Module reloaded"))

(defun geiser-insert-lambda ()
  "Insert lambda symbol (λ)."
  (interactive)
  (insert "λ"))

;; Bind helpers
(with-eval-after-load 'geiser-mode
  (define-key geiser-mode-map (kbd "C-c C-l") 'geiser-reload-module)
  (define-key geiser-mode-map (kbd "C-c λ") 'geiser-insert-lambda))

;;; ============================================================================
;;; REPL Customization
;;; ============================================================================

(defun my-geiser-repl-mode-hook ()
  "Custom Geiser REPL setup."
  ;; Enable line numbers
  (display-line-numbers-mode -1)  ; Disable in REPL

  ;; Set REPL window height
  (when (window-live-p (selected-window))
    (enlarge-window 10)))

(add-hook 'geiser-repl-mode-hook 'my-geiser-repl-mode-hook)

;;; ============================================================================
;;; Multiple Scheme Implementations (Optional)
;;; ============================================================================

;; Uncomment to enable additional Scheme implementations:

;; Racket support
;; (when (package-installed-p 'geiser-racket)
;;   (require 'geiser-racket)
;;   (setq geiser-racket-binary "racket"))

;; Chicken support
;; (when (package-installed-p 'geiser-chicken)
;;   (require 'geiser-chicken)
;;   (setq geiser-chicken-binary "csi"))

;; Chez support
;; (when (package-installed-p 'geiser-chez)
;;   (require 'geiser-chez)
;;   (setq geiser-chez-binary "chez"))

;;; ============================================================================
;;; Image Display Support (Optional)
;;; ============================================================================

(when (and (display-graphic-p)
           (package-installed-p 'geiser-guile))
  ;; Enable inline image display
  (setq geiser-image-viewer 'imagemagick))

;;; ============================================================================
;;; Experiment-Specific Settings
;;; ============================================================================

;; Auto-load Geiser for .scm files
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-hook 'scheme-mode-hook 'geiser-mode)

;; Show welcome message
(defun geiser-experiment-welcome ()
  "Display welcome message for Experiment 076."
  (message "Geiser Experiment 076 loaded! Use M-x run-guile to start REPL."))

(add-hook 'emacs-startup-hook 'geiser-experiment-welcome)

;;; ============================================================================
;;; Quick Reference
;;; ============================================================================

;; Key Bindings Quick Reference:
;; C-c C-z    - Switch to REPL
;; C-c C-b    - Eval buffer
;; C-c C-r    - Eval region
;; C-c C-e    - Eval last sexp
;; C-c C-c    - Eval definition
;; M-.        - Jump to definition
;; M-,        - Pop back from definition
;; C-c C-d d  - Show documentation
;; C-c C-d m  - Show module documentation
;; C-c C-m x  - Expand last sexp
;; C-c C-q    - Quit REPL

(provide 'init-geiser)
;;; init-geiser.el ends here
