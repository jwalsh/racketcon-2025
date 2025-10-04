;;; racketcon-2025-config.el --- Pure Elisp configuration for RacketCon 2025 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon

;; Author: RacketCon 2025
;; Keywords: racket, tools
;; Version: 1.0.0

;;; Commentary:

;; Pure Elisp configuration file for RacketCon 2025 experiments.
;; Load this file to configure Racket mode for this host.
;;
;; Usage:
;;   (load "/Users/jasonwalsh/ghq/github.com/jwalsh/racketcon-2025/racketcon-2025-config.el")
;;
;; Or add to init.el:
;;   (when (file-exists-p "/Users/jasonwalsh/ghq/github.com/jwalsh/racketcon-2025/racketcon-2025-config.el")
;;     (load "/Users/jasonwalsh/ghq/github.com/jwalsh/racketcon-2025/racketcon-2025-config.el"))

;;; Code:

;; ============================================================================
;; System Detection
;; ============================================================================

(defconst racketcon-system-type
  (cond ((eq system-type 'darwin) 'macos)
        ((eq system-type 'gnu/linux) 'linux)
        ((eq system-type 'berkeley-unix) 'freebsd)
        (t 'unknown))
  "Detected system type.")

(defconst racketcon-on-macos (eq racketcon-system-type 'macos))
(defconst racketcon-on-freebsd (eq racketcon-system-type 'freebsd))

;; ============================================================================
;; Racket Installation Paths
;; ============================================================================

(defvar racketcon-racket-program
  (cond
   ;; macOS: Try standard locations
   (racketcon-on-macos
    (or (executable-find "racket")
        "/Applications/Racket v8.15/bin/racket"
        "/Applications/Racket v8.17/bin/racket"
        "/usr/local/bin/racket"
        "/opt/homebrew/bin/racket"))
   ;; FreeBSD: pkg install locations
   (racketcon-on-freebsd
    (or (executable-find "racket")
        "/usr/local/bin/racket"))
   ;; Other Unix
   (t (executable-find "racket")))
  "Path to Racket executable.")

(defvar racketcon-raco-program
  (when racketcon-racket-program
    (let ((dir (file-name-directory racketcon-racket-program)))
      (concat dir "raco")))
  "Path to raco executable.")

;; ============================================================================
;; Project Paths
;; ============================================================================

(defvar racketcon-project-root
  "/Users/jasonwalsh/ghq/github.com/jwalsh/racketcon-2025"
  "Root directory of RacketCon 2025 project.")

(defvar racketcon-experiments-dir
  (expand-file-name "experiments" racketcon-project-root)
  "Directory containing experiments.")

(defvar racketcon-elisp-dir
  (expand-file-name "elisp" racketcon-project-root)
  "Directory containing Elisp tools.")

;; ============================================================================
;; Load Path Setup
;; ============================================================================

(when (file-directory-p racketcon-elisp-dir)
  (add-to-list 'load-path racketcon-elisp-dir))

;; ============================================================================
;; Racket Mode Configuration
;; ============================================================================

(defun racketcon-setup-racket-mode ()
  "Configure racket-mode for RacketCon 2025."
  (when (require 'racket-mode nil t)

    ;; Set Racket program paths
    (when racketcon-racket-program
      (setq racket-program racketcon-racket-program))
    (when racketcon-raco-program
      (setq racket-raco-program racketcon-raco-program))

    ;; Enable racket-xp-mode for IDE features
    (add-hook 'racket-mode-hook #'racket-xp-mode)

    ;; Custom indentation for RHEA framework
    (put 'define-hypothesis 'racket-indent-function 1)
    (put 'run-experiment 'racket-indent-function 1)
    (put 'analyze-results 'racket-indent-function 1)
    (put 'define-2d 'racket-indent-function 1)
    (put 'struct-lens 'racket-indent-function 1)
    (put 'make-prism 'racket-indent-function 2)
    (put 'make-traversal 'racket-indent-function 2)

    ;; REPL settings
    (setq racket-repl-buffer-name-function #'racket-repl-buffer-name-project)
    (setq racket-documentation-search-location 'local)

    ;; Performance tuning
    (setq racket-xp-after-change-refresh-delay 1)

    (message "RacketCon 2025: Racket mode configured")))

;; ============================================================================
;; Supporting Modes
;; ============================================================================

(defun racketcon-setup-supporting-modes ()
  "Configure supporting modes for better Racket experience."

  ;; Smartparens for balanced parens
  (when (require 'smartparens nil t)
    (add-hook 'racket-mode-hook #'smartparens-strict-mode))

  ;; Rainbow delimiters for visual matching
  (when (require 'rainbow-delimiters nil t)
    (add-hook 'racket-mode-hook #'rainbow-delimiters-mode))

  ;; Company mode for completion
  (when (require 'company nil t)
    (add-hook 'racket-mode-hook #'company-mode))

  ;; Line numbers
  (add-hook 'racket-mode-hook #'display-line-numbers-mode)

  (message "RacketCon 2025: Supporting modes configured"))

;; ============================================================================
;; Org-Babel Configuration
;; ============================================================================

(defun racketcon-setup-org-babel ()
  "Configure org-babel for literate programming."
  (when (require 'org nil t)
    (require 'ob-racket nil t)

    ;; Enable Racket in org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((racket . t)
       (emacs-lisp . t)
       (shell . t)
       (python . t)))

    ;; Don't ask for confirmation
    (setq org-confirm-babel-evaluate nil)

    ;; Syntax highlighting in source blocks
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-src-preserve-indentation t)

    (message "RacketCon 2025: Org-babel configured")))

;; ============================================================================
;; RacketCon Tools Integration
;; ============================================================================

(defun racketcon-load-tools ()
  "Load racketcon-tools.el if available."
  (let ((tools-file (expand-file-name "racketcon-tools.el" racketcon-elisp-dir)))
    (when (file-exists-p tools-file)
      (load tools-file)
      (when (fboundp 'racketcon-mode-enable)
        (add-hook 'racket-mode-hook #'racketcon-mode-enable)
        (add-hook 'org-mode-hook #'racketcon-mode-enable))
      (message "RacketCon 2025: Tools loaded"))))

;; ============================================================================
;; Quick Access Functions
;; ============================================================================

(defun racketcon-goto-project ()
  "Open RacketCon 2025 project root."
  (interactive)
  (dired racketcon-project-root))

(defun racketcon-goto-experiments ()
  "Open experiments directory."
  (interactive)
  (dired racketcon-experiments-dir))

(defun racketcon-open-session-notes ()
  "Open sessions.org."
  (interactive)
  (find-file (expand-file-name "sessions.org" racketcon-project-root)))

(defun racketcon-verify-setup ()
  "Verify RacketCon 2025 setup."
  (interactive)
  (with-output-to-temp-buffer "*RacketCon Setup*"
    (princ "=== RacketCon 2025 Setup Verification ===\n\n")

    (princ "System:\n")
    (princ (format "  Type: %s\n" racketcon-system-type))
    (princ (format "  Emacs: %s\n\n" emacs-version))

    (princ "Racket:\n")
    (princ (format "  Program: %s\n"
                   (if (and racketcon-racket-program
                            (file-exists-p racketcon-racket-program))
                       (concat racketcon-racket-program " ✓")
                     "Not found ✗")))
    (princ (format "  Raco: %s\n\n"
                   (if (and racketcon-raco-program
                            (file-exists-p racketcon-raco-program))
                       (concat racketcon-raco-program " ✓")
                     "Not found ✗")))

    (princ "Project:\n")
    (princ (format "  Root: %s\n"
                   (if (file-directory-p racketcon-project-root)
                       (concat racketcon-project-root " ✓")
                     "Not found ✗")))
    (princ (format "  Experiments: %s\n\n"
                   (if (file-directory-p racketcon-experiments-dir)
                       (concat racketcon-experiments-dir " ✓")
                     "Not found ✗")))

    (princ "Modes:\n")
    (princ (format "  racket-mode: %s\n"
                   (if (featurep 'racket-mode) "Loaded ✓" "Not loaded ✗")))
    (princ (format "  smartparens: %s\n"
                   (if (featurep 'smartparens) "Loaded ✓" "Not loaded ✗")))
    (princ (format "  rainbow-delimiters: %s\n"
                   (if (featurep 'rainbow-delimiters) "Loaded ✓" "Not loaded ✗")))
    (princ (format "  company: %s\n\n"
                   (if (featurep 'company) "Loaded ✓" "Not loaded ✗")))

    (princ "Tools:\n")
    (princ (format "  racketcon-tools: %s\n"
                   (if (featurep 'racketcon-tools) "Loaded ✓" "Not loaded ✗")))

    (princ "\nReady for RacketCon 2025!\n")))

;; ============================================================================
;; Key Bindings
;; ============================================================================

(global-set-key (kbd "C-c R p") #'racketcon-goto-project)
(global-set-key (kbd "C-c R e") #'racketcon-goto-experiments)
(global-set-key (kbd "C-c R s") #'racketcon-open-session-notes)
(global-set-key (kbd "C-c R v") #'racketcon-verify-setup)

;; ============================================================================
;; Auto-Setup
;; ============================================================================

(defun racketcon-setup-all ()
  "Run all RacketCon 2025 setup functions."
  (interactive)
  (racketcon-setup-racket-mode)
  (racketcon-setup-supporting-modes)
  (racketcon-setup-org-babel)
  (racketcon-load-tools)
  (message "RacketCon 2025: Complete setup finished!"))

;; Run setup automatically when loaded
(racketcon-setup-all)

;; ============================================================================
;; Provide Feature
;; ============================================================================

(provide 'racketcon-2025-config)

;;; racketcon-2025-config.el ends here
