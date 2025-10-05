#!/usr/bin/env emacs --script
;; lint-org.el - Lint all org files in repository using org-lint

(require 'org)
(require 'org-lint)

(defun lint-org-file (file)
  "Lint an org FILE and return results."
  (message "Linting: %s" file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((warnings (org-lint)))
      (if warnings
          (progn
            (message "Issues found in %s:" file)
            (dolist (warning warnings)
              (message "  Line %d: %s - %s"
                       (org-lint-warning-line warning)
                       (org-lint-warning-type warning)
                       (org-lint-warning-message warning)))
            warnings)
        (message "  ✓ No issues")
        nil))))

(defun lint-all-org-files (directory)
  "Recursively lint all .org files in DIRECTORY."
  (let ((org-files (directory-files-recursively directory "\\.org$"))
        (total-warnings 0)
        (files-with-issues 0))
    (message "=== Linting Org Files ===\n")
    (dolist (file org-files)
      (let ((warnings (lint-org-file file)))
        (when warnings
          (setq total-warnings (+ total-warnings (length warnings)))
          (setq files-with-issues (1+ files-with-issues)))))
    (message "\n=== Summary ===")
    (message "Files checked: %d" (length org-files))
    (message "Files with issues: %d" files-with-issues)
    (message "Total warnings: %d" total-warnings)
    (if (> total-warnings 0)
        (kill-emacs 1)
      (message "\n✓ All org files are clean!")
      (kill-emacs 0))))

;; Main
(let ((repo-root (or (getenv "REPO_ROOT")
                     (file-name-directory
                      (directory-file-name
                       (file-name-directory load-file-name))))))
  (lint-all-org-files repo-root))
