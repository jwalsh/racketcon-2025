;;; racketcon-loader.el --- Incremental loader for RacketCon 2025 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon

;; Author: RacketCon 2025
;; Keywords: racket, tools
;; Version: 1.0.0

;;; Commentary:

;; This file provides incremental loading of RacketCon 2025 Elisp modules.
;; It loads supporting files from elisp/ directory in a clean, testable way.

;;; Code:

(defvar racketcon-loader-loaded-modules nil
  "List of successfully loaded RacketCon modules.")

(defvar racketcon-loader-failed-modules nil
  "List of modules that failed to load with error messages.")

(defvar racketcon-elisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing RacketCon Elisp files.")

(defun racketcon-loader-load-module (module-name &optional required)
  "Load MODULE-NAME from elisp directory.
If REQUIRED is non-nil, signal an error on failure.
Otherwise, log the error and continue."
  (let ((module-file (expand-file-name (concat module-name ".el") racketcon-elisp-dir)))
    (condition-case err
        (progn
          (load module-file nil 'nomessage)
          (add-to-list 'racketcon-loader-loaded-modules module-name)
          (message "✓ Loaded %s" module-name)
          t)
      (error
       (let ((err-msg (format "Failed to load %s: %s" module-name (error-message-string err))))
         (add-to-list 'racketcon-loader-failed-modules (cons module-name err-msg))
         (if required
             (error err-msg)
           (message "✗ %s (optional)" err-msg))
         nil)))))

(defun racketcon-loader-load-all ()
  "Load all RacketCon modules incrementally."
  (interactive)
  (message "Loading RacketCon 2025 modules...")

  ;; Reset tracking lists
  (setq racketcon-loader-loaded-modules nil)
  (setq racketcon-loader-failed-modules nil)

  ;; Load modules in order (optional modules won't fail)
  (racketcon-loader-load-module "racketcon-tools" nil)
  (racketcon-loader-load-module "ion-mode" nil)

  ;; Report results
  (message "RacketCon loader: %d loaded, %d failed"
           (length racketcon-loader-loaded-modules)
           (length racketcon-loader-failed-modules))

  ;; Return success status
  (null racketcon-loader-failed-modules))

(defun racketcon-loader-status ()
  "Show loading status of RacketCon modules."
  (interactive)
  (with-output-to-temp-buffer "*RacketCon Loader Status*"
    (princ "=== RacketCon 2025 Module Loader Status ===\n\n")

    (princ "Elisp Directory:\n")
    (princ (format "  %s\n\n" racketcon-elisp-dir))

    (princ "Loaded Modules:\n")
    (if racketcon-loader-loaded-modules
        (dolist (module racketcon-loader-loaded-modules)
          (princ (format "  ✓ %s\n" module)))
      (princ "  (none)\n"))
    (princ "\n")

    (princ "Failed Modules:\n")
    (if racketcon-loader-failed-modules
        (dolist (module-err racketcon-loader-failed-modules)
          (princ (format "  ✗ %s: %s\n" (car module-err) (cdr module-err))))
      (princ "  (none)\n"))
    (princ "\n")

    (princ (format "Total: %d loaded, %d failed\n"
                   (length racketcon-loader-loaded-modules)
                   (length racketcon-loader-failed-modules)))))

(defun racketcon-loader-reload-all ()
  "Reload all RacketCon modules."
  (interactive)
  (racketcon-loader-load-all))

(provide 'racketcon-loader)

;;; racketcon-loader.el ends here
