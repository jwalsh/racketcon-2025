#!/usr/bin/env emacs --script
;; lint-org.el - Lint all org files in repository using org-lint

(require 'org)
(require 'org-lint)

(defun lint-org-file (file)
  "Lint an org FILE and return number of warnings."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((warnings (org-lint)))
      (when warnings
        (message "  %s: %d warnings"
                 (file-relative-name file default-directory)
                 (length warnings)))
      (length warnings))))

(defun lint-all-org-files (directory)
  "Recursively lint all .org files in DIRECTORY."
  (let ((org-files (directory-files-recursively directory "\\.org$"))
        (total-warnings 0))
    (message "=== Linting Org Files ===")
    (message "Checking %d files...\n" (length org-files))
    (dolist (file org-files)
      (setq total-warnings (+ total-warnings (lint-org-file file))))
    (message "\n=== Summary ===")
    (message "Files checked: %d" (length org-files))
    (message "Total warnings: %d" total-warnings)
    (if (> total-warnings 0)
        (progn
          (message "\n⚠ Found %d warnings" total-warnings)
          (kill-emacs 1))
      (message "\n✓ All org files are clean!")
      (kill-emacs 0))))

;; Main
(let ((repo-root (or (getenv "REPO_ROOT")
                     (file-name-directory
                      (directory-file-name
                       (file-name-directory load-file-name))))))
  (lint-all-org-files repo-root))
