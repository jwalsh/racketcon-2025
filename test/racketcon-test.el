;;; racketcon-test.el --- ERT tests for RacketCon 2025 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon

;; Author: RacketCon 2025
;; Keywords: tests
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (ert "0"))

;;; Commentary:

;; ERT tests for RacketCon 2025 configuration and tools.
;;
;; Run tests:
;;   M-x ert RET t RET
;; Or from command line:
;;   emacs -batch -l test/racketcon-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Add project directories to load path
(let ((project-root (locate-dominating-file default-directory "racketcon-2025-config.el")))
  (when project-root
    (add-to-list 'load-path project-root)
    (add-to-list 'load-path (expand-file-name "elisp" project-root))))

;; Load the modules we're testing
(require 'racketcon-loader)

;;; ============================================================================
;;; System Detection Tests
;;; ============================================================================

(ert-deftest racketcon-test-system-detection ()
  "Test that system type is detected correctly."
  (should (boundp 'racketcon-system-type))
  (should (memq racketcon-system-type '(macos linux freebsd unknown))))

(ert-deftest racketcon-test-freebsd-detection ()
  "Test FreeBSD detection on current system."
  (should (boundp 'racketcon-on-freebsd))
  ;; On FreeBSD, this should be t
  (when (eq system-type 'berkeley-unix)
    (should (eq racketcon-on-freebsd t))))

;;; ============================================================================
;;; Path Tests
;;; ============================================================================

(ert-deftest racketcon-test-project-root-exists ()
  "Test that project root is set and exists."
  (should (boundp 'racketcon-project-root))
  (should (stringp racketcon-project-root))
  (should (file-directory-p racketcon-project-root)))

(ert-deftest racketcon-test-elisp-dir-exists ()
  "Test that elisp directory exists."
  (should (boundp 'racketcon-elisp-dir))
  (should (stringp racketcon-elisp-dir))
  (should (file-directory-p racketcon-elisp-dir)))

(ert-deftest racketcon-test-experiments-dir-exists ()
  "Test that experiments directory exists."
  (should (boundp 'racketcon-experiments-dir))
  (should (stringp racketcon-experiments-dir))
  (should (file-directory-p racketcon-experiments-dir)))

;;; ============================================================================
;;; Racket Executable Tests
;;; ============================================================================

(ert-deftest racketcon-test-racket-program-found ()
  "Test that Racket executable is found."
  (should (boundp 'racketcon-racket-program))
  (should (or (null racketcon-racket-program)
              (file-executable-p racketcon-racket-program))))

(ert-deftest racketcon-test-raco-program-found ()
  "Test that raco executable is found."
  (should (boundp 'racketcon-raco-program))
  (when racketcon-raco-program
    (should (file-executable-p racketcon-raco-program))))

;;; ============================================================================
;;; Loader Tests
;;; ============================================================================

(ert-deftest racketcon-test-loader-variables ()
  "Test that loader variables are defined."
  (should (boundp 'racketcon-loader-loaded-modules))
  (should (boundp 'racketcon-loader-failed-modules))
  (should (boundp 'racketcon-elisp-dir)))

(ert-deftest racketcon-test-loader-functions ()
  "Test that loader functions are defined."
  (should (fboundp 'racketcon-loader-load-module))
  (should (fboundp 'racketcon-loader-load-all))
  (should (fboundp 'racketcon-loader-status))
  (should (fboundp 'racketcon-loader-reload-all)))

(ert-deftest racketcon-test-loader-load-nonexistent ()
  "Test loader behavior with nonexistent module."
  (let ((racketcon-loader-loaded-modules nil)
        (racketcon-loader-failed-modules nil))
    (should-not (racketcon-loader-load-module "nonexistent-module-xyz" nil))
    (should (= (length racketcon-loader-failed-modules) 1))))

;;; ============================================================================
;;; Configuration Function Tests
;;; ============================================================================

(ert-deftest racketcon-test-setup-functions-exist ()
  "Test that setup functions are defined."
  (should (fboundp 'racketcon-setup-geiser))
  (should (fboundp 'racketcon-setup-racket-mode))
  (should (fboundp 'racketcon-setup-supporting-modes))
  (should (fboundp 'racketcon-setup-org-babel))
  (should (fboundp 'racketcon-load-tools))
  (should (fboundp 'racketcon-setup-ion-mode))
  (should (fboundp 'racketcon-setup-all)))

(ert-deftest racketcon-test-utility-functions-exist ()
  "Test that utility functions are defined."
  (should (fboundp 'racketcon-goto-project))
  (should (fboundp 'racketcon-goto-experiments))
  (should (fboundp 'racketcon-open-session-notes))
  (should (fboundp 'racketcon-verify-setup)))

;;; ============================================================================
;;; Key Binding Tests
;;; ============================================================================

(ert-deftest racketcon-test-key-bindings ()
  "Test that RacketCon key bindings are set."
  (should (keymapp (lookup-key global-map (kbd "C-c R"))))
  (should (commandp (lookup-key global-map (kbd "C-c R p"))))
  (should (commandp (lookup-key global-map (kbd "C-c R e"))))
  (should (commandp (lookup-key global-map (kbd "C-c R s"))))
  (should (commandp (lookup-key global-map (kbd "C-c R v")))))

;;; ============================================================================
;;; Mode Configuration Tests
;;; ============================================================================

(ert-deftest racketcon-test-racket-mode-available ()
  "Test if racket-mode is available or can be loaded."
  (should (or (featurep 'racket-mode)
              (require 'racket-mode nil t))))

(ert-deftest racketcon-test-geiser-available ()
  "Test if geiser is available or can be loaded."
  (should (or (featurep 'geiser)
              (require 'geiser nil t))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(ert-deftest racketcon-test-goto-project ()
  "Test goto-project function."
  (should (commandp 'racketcon-goto-project))
  ;; Don't actually change buffer, just verify it's callable
  (should (functionp 'racketcon-goto-project)))

(ert-deftest racketcon-test-verify-setup-callable ()
  "Test that verify-setup is callable."
  (should (commandp 'racketcon-verify-setup))
  ;; Call it and check it doesn't error
  (should-not (condition-case err
                  (progn (racketcon-verify-setup) nil)
                (error t))))

;;; ============================================================================
;;; File Existence Tests
;;; ============================================================================

(ert-deftest racketcon-test-config-file-exists ()
  "Test that main config file exists."
  (let ((config-file (expand-file-name "racketcon-2025-config.el" racketcon-project-root)))
    (should (file-exists-p config-file))))

(ert-deftest racketcon-test-dir-locals-exists ()
  "Test that .dir-locals.el exists."
  (let ((dir-locals (expand-file-name ".dir-locals.el" racketcon-project-root)))
    (should (file-exists-p dir-locals))))

(ert-deftest racketcon-test-sessions-org-exists ()
  "Test that sessions.org exists."
  (let ((sessions-file (expand-file-name "sessions.org" racketcon-project-root)))
    (should (file-exists-p sessions-file))))

;;; ============================================================================
;;; Load Path Tests
;;; ============================================================================

(ert-deftest racketcon-test-elisp-in-load-path ()
  "Test that elisp directory is in load path."
  (should (member racketcon-elisp-dir load-path)))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun racketcon-run-all-tests ()
  "Run all RacketCon tests interactively."
  (interactive)
  (ert "^racketcon-test-"))

(defun racketcon-test-summary ()
  "Display test summary."
  (interactive)
  (with-output-to-temp-buffer "*RacketCon Test Summary*"
    (princ "=== RacketCon 2025 Test Summary ===\n\n")
    (princ (format "Total tests defined: %d\n"
                   (length (ert-select-tests "^racketcon-test-" t))))
    (princ "\nRun tests with:\n")
    (princ "  M-x racketcon-run-all-tests\n")
    (princ "  M-x ert RET racketcon-test- RET\n")
    (princ "\nOr from command line:\n")
    (princ "  emacs -batch -l test/racketcon-test.el -f ert-run-tests-batch-and-exit\n")))

(provide 'racketcon-test)

;;; racketcon-test.el ends here
