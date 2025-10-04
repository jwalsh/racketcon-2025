;;; .dir-locals.el --- Directory-local variables for RacketCon 2025 experiments -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon

;; This file contains Emacs configuration for the RacketCon 2025 experiments
;; and RHEA framework.

;;; Commentary:

;; This provides:
;; - Racket mode configuration
;; - Org-mode integration for literate programming
;; - RHEA framework support
;; - Experiment navigation
;; - 2D syntax support

;;; Code:

((nil . ((indent-tabs-mode . nil)
         (fill-column . 80)
         (eval . (progn
                   ;; Add project root to load path
                   (add-to-list 'load-path
                               (expand-file-name "elisp"
                                                (locate-dominating-file
                                                 default-directory
                                                 ".dir-locals.el")))

                   ;; Set up RHEA environment
                   (setq rhea-project-root
                         (locate-dominating-file default-directory
                                               ".dir-locals.el"))

                   ;; Configure experiment paths
                   (setq racketcon-experiments-dir
                         (expand-file-name "experiments" rhea-project-root))))))

 (racket-mode . ((eval . (progn
                          (require 'racket-mode nil t)
                          (require 'racket-xp-mode nil t)

                          ;; Enable racket-xp-mode for all racket files
                          (racket-xp-mode 1)

                          ;; Set up REPL
                          (setq racket-program "racket")
                          (setq racket-documentation-search-location 'local)

                          ;; Enable smart parens
                          (when (fboundp 'smartparens-mode)
                            (smartparens-mode 1))

                          ;; Enable rainbow delimiters
                          (when (fboundp 'rainbow-delimiters-mode)
                            (rainbow-delimiters-mode 1))

                          ;; Configure indentation
                          (put 'define-hypothesis 'racket-indent-function 1)
                          (put 'run-experiment 'racket-indent-function 1)
                          (put 'analyze-results 'racket-indent-function 1)
                          (put 'define-2d 'racket-indent-function 1)
                          (put 'struct-lens 'racket-indent-function 1)
                          (put 'make-prism 'racket-indent-function 2)
                          (put 'make-traversal 'racket-indent-function 2)))))

 (org-mode . ((eval . (progn
                       (require 'ob-racket nil t)
                       (require 'ob-shell nil t)
                       (require 'ob-emacs-lisp nil t)

                       ;; Enable org-babel for racket
                       (org-babel-do-load-languages
                        'org-babel-load-languages
                        '((racket . t)
                          (shell . t)
                          (emacs-lisp . t)
                          (ruby . t)
                          (python . t)))

                       ;; Don't ask for confirmation
                       (setq org-confirm-babel-evaluate nil)

                       ;; Enable syntax highlighting in source blocks
                       (setq org-src-fontify-natively t)
                       (setq org-src-tab-acts-natively t)
                       (setq org-src-preserve-indentation t)

                       ;; Configure export
                       (setq org-export-with-toc t)
                       (setq org-export-with-author t)
                       (setq org-export-with-date t)

                       ;; RHEA-specific settings
                       (setq org-todo-keywords
                             '((sequence "HYPOTHESIS" "EXPERIMENT" "ANALYSIS" "|" "SUPPORTED" "REJECTED")
                               (sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELLED")))

                       ;; Enable mermaid diagrams
                       (when (fboundp 'org-babel-execute:mermaid)
                         (add-to-list 'org-babel-load-languages '(mermaid . t)))))))

 (ruby-mode . ((eval . (progn
                        ;; Ruby lens library settings
                        (setq ruby-indent-level 2)
                        (setq ruby-deep-indent-paren nil)

                        ;; Enable RuboCop if available
                        (when (fboundp 'rubocop-mode)
                          (rubocop-mode 1))))))

 (markdown-mode . ((markdown-fontify-code-blocks-natively . t)))

 (emacs-lisp-mode . ((checkdoc-minor-mode . t))))

;;; .dir-locals.el ends here
