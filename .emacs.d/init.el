;;; init.el --- RacketCon 2025 Project Configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author: RacketCon 2025 Project
;; Maintainer: RacketCon 2025 Project
;; Created: October 05, 2025
;; Modified: October 05, 2025
;; Version: 0.0.1
;; Keywords: racket lisp
;; Homepage: https://github.com/jwalsh/racketcon-2025
;; Package-Requires: ((emacs "30.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Project-specific Emacs configuration for RacketCon 2025
;; Requires Emacs 30+
;;
;;; Code:

;; Ensure minimum Emacs version
(when (version< emacs-version "30.1")
  (error "This configuration requires Emacs 30.1 or higher"))

;; Initialize package.el
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Geiser for Racket
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(racket guile)))

(use-package geiser-racket
  :ensure t
  :config
  (setq geiser-racket-binary "racket")
  (setq geiser-racket-use-gracket-p nil)
  ;; Set Racket program location
  (setq geiser-racket-executable (executable-find "racket")))

(use-package geiser-guile
  :ensure t
  :config
  (setq geiser-guile-binary "guile3"))

;; Racket mode
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'"
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . rainbow-delimiters-mode))
  :config
  (setq racket-program "racket")
  (setq racket-repl-buffer-name-function
        (lambda () "*Racket REPL*")))

;; Rhombus support
(use-package racket-mode
  :mode ("\\.rhm\\'" . racket-mode))

;; Paredit for structural editing
(use-package paredit
  :ensure t
  :hook ((racket-mode . paredit-mode)
         (racket-repl-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (geiser-repl-mode . paredit-mode)))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook ((racket-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (geiser-repl-mode . rainbow-delimiters-mode)))

;; Company for completion
(use-package company
  :ensure t
  :hook ((racket-mode . company-mode)
         (racket-repl-mode . company-mode)
         (geiser-repl-mode . company-mode))
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

;; Projectile for project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/ghq/github.com/jwalsh/")))

;; Magit for Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :hook ((racket-mode . flycheck-mode)))

;; Which-key for discoverability
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Project-specific settings
(setq default-directory "/home/jwalsh/ghq/github.com/jwalsh/racketcon-2025/")

;; Set up load paths for local Racket files
(add-to-list 'load-path (expand-file-name "src" default-directory))
(add-to-list 'load-path (expand-file-name "lib" default-directory))

;; Auto-revert for git changes
(global-auto-revert-mode t)

;; Useful keybindings
(global-set-key (kbd "C-c C-z") 'geiser-mode-switch-to-repl-and-enter)
(global-set-key (kbd "C-c C-k") 'geiser-compile-current-buffer)

;; Display settings
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(provide 'init)
;;; init.el ends here
