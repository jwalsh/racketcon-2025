;;; racketcon-tools.el --- Tools for RacketCon 2025 experiments -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon

;; Author: RacketCon 2025
;; Keywords: racket, tools, literate-programming, rhea
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (racket-mode "1.0"))

;;; Commentary:

;; This package provides tools for working with RacketCon 2025 experiments
;; and the RHEA (Racket Hypothesis-Experiment-Analysis) framework.
;;
;; Features:
;; - Navigate experiments
;; - Run RHEA framework
;; - Export to org-mode
;; - Literate programming support
;; - Lens/optics helpers
;; - 2D syntax support

;;; Code:

(require 'racket-mode)
(require 'org)
(require 'cl-lib)

;;; Customization

(defgroup racketcon nil
  "RacketCon 2025 experiment tools."
  :group 'racket)

(defcustom racketcon-experiments-dir
  (expand-file-name "experiments" (locate-dominating-file default-directory ".git"))
  "Directory containing RacketCon experiments."
  :type 'directory
  :group 'racketcon)

(defcustom racketcon-rhea-enabled t
  "Enable RHEA framework integration."
  :type 'boolean
  :group 'racketcon)

;;; Experiment Navigation

(defun racketcon-list-experiments ()
  "List all RacketCon experiments."
  (interactive)
  (let ((experiments (directory-files racketcon-experiments-dir t "^[0-9]\\{3\\}-.*")))
    (with-current-buffer (get-buffer-create "*RacketCon Experiments*")
      (erase-buffer)
      (insert "# RacketCon 2025 Experiments\n\n")
      (dolist (exp (sort experiments #'string<))
        (when (file-directory-p exp)
          (let* ((name (file-name-nondirectory exp))
                 (readme (expand-file-name "README.org" exp))
                 (desc (when (file-exists-p readme)
                        (with-temp-buffer
                          (insert-file-contents readme)
                          (goto-char (point-min))
                          (when (re-search-forward "^\\*\\* Overview\\s-*\n\\(.*\\)" nil t)
                            (match-string 1))))))
            (insert (format "- [[file:%s][%s]]: %s\n"
                          exp name (or desc "No description"))))))
      (org-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun racketcon-goto-experiment (number)
  "Go to experiment NUMBER."
  (interactive "nExperiment number: ")
  (let* ((exp-name (format "%03d-*" number))
         (exp-dir (car (directory-files racketcon-experiments-dir t exp-name))))
    (if exp-dir
        (dired exp-dir)
      (message "Experiment %d not found" number))))

(defun racketcon-new-experiment (number name)
  "Create new experiment NUMBER with NAME."
  (interactive "nExperiment number: \nsExperiment name: ")
  (let* ((dir-name (format "%03d-%s" number (replace-regexp-in-string " " "-" name)))
         (exp-dir (expand-file-name dir-name racketcon-experiments-dir)))
    (make-directory exp-dir t)
    (with-temp-file (expand-file-name "README.org" exp-dir)
      (insert (format "#+TITLE: Experiment %03d: %s\n" number name))
      (insert "#+AUTHOR: RacketCon 2025\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "* Overview\n\n")
      (insert "* Goals\n\n")
      (insert "* Implementation\n\n")
      (insert "* Results\n\n"))
    (find-file (expand-file-name "README.org" exp-dir))))

;;; RHEA Framework Integration

(defun racketcon-rhea-new-hypothesis (name)
  "Create new RHEA hypothesis with NAME."
  (interactive "sHypothesis name: ")
  (insert "(define-hypothesis " name "\n")
  (insert "  [#:description \"\"]\n")
  (insert "  [#:assumes \"\"]\n")
  (insert "  [#:predicts \"\"]\n")
  (insert "  [#:metadata 'domain 'unknown\n")
  (insert "              'status 'proposed])\n")
  (forward-line -5)
  (end-of-line)
  (backward-char 1))

(defun racketcon-rhea-new-experiment (name)
  "Create new RHEA experiment with NAME."
  (interactive "sExperiment name: ")
  (insert "(run-experiment " name "\n")
  (insert "  [#:tests hypothesis]\n")
  (insert "  [#:setup (λ () (hash))]\n")
  (insert "  [#:procedure (λ (env) )]\n")
  (insert "  [#:collect identity]\n")
  (insert "  [#:replicate 100])\n")
  (forward-line -4)
  (end-of-line)
  (backward-char 2))

(defun racketcon-rhea-export-org ()
  "Export current RHEA analysis to org-mode."
  (interactive)
  (racket-send-region (point-min) (point-max))
  (racket-repl-send-string "(export-to-org hypothesis (list experiment) (list results))"))

;;; Lens/Optics Helpers

(defun racketcon-insert-lens ()
  "Insert lens template."
  (interactive)
  (insert "(define name-lens\n")
  (insert "  (lens\n")
  (insert "    getter\n")
  (insert "    setter))\n")
  (forward-line -3)
  (forward-char 8))

(defun racketcon-insert-prism ()
  "Insert prism template."
  (interactive)
  (insert "(define name-prism\n")
  (insert "  (prism\n")
  (insert "    match?\n")
  (insert "    getter\n")
  (insert "    setter))\n")
  (forward-line -4)
  (forward-char 8))

(defun racketcon-insert-traversal ()
  "Insert traversal template."
  (interactive)
  (insert "(define name-traversal\n")
  (insert "  (traversal\n")
  (insert "    to-list\n")
  (insert "    over))\n")
  (forward-line -3)
  (forward-char 8))

(defun racketcon-verify-lens-laws ()
  "Insert lens law verification code."
  (interactive)
  (insert ";; Lens Law 1: GetPut\n")
  (insert "(check-equal? (set l target (view l target)) target)\n\n")
  (insert ";; Lens Law 2: PutGet\n")
  (insert "(check-equal? (view l (set l target value)) value)\n\n")
  (insert ";; Lens Law 3: PutPut\n")
  (insert "(check-equal? (set l (set l target v1) v2) (set l target v2))\n"))

;;; 2D Syntax Support

(defun racketcon-insert-2d-grid (rows cols)
  "Insert 2D grid with ROWS and COLS."
  (interactive "nRows: \nCols: ")
  (insert "╔")
  (dotimes (_ (1- cols)) (insert "═══════════╦"))
  (insert "═══════════╗\n")

  (dotimes (r rows)
    (insert "║")
    (dotimes (c cols)
      (insert (format " var%d%d      " r c))
      (unless (= c (1- cols))
        (insert "║")))
    (insert "║\n")

    (unless (= r (1- rows))
      (insert "╠")
      (dotimes (_ (1- cols)) (insert "═══════════╬"))
      (insert "═══════════╣\n")))

  (insert "╚")
  (dotimes (_ (1- cols)) (insert "═══════════╩"))
  (insert "═══════════╝\n"))

;;; Org-Mode Integration

(defun racketcon-org-export-experiments ()
  "Export all experiments to a single org file."
  (interactive)
  (let ((output-file (expand-file-name "ALL-EXPERIMENTS.org" racketcon-experiments-dir))
        (experiments (directory-files racketcon-experiments-dir t "^[0-9]\\{3\\}-.*")))
    (with-temp-file output-file
      (insert "#+TITLE: RacketCon 2025: All Experiments\n")
      (insert "#+AUTHOR: RacketCon 2025\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "#+OPTIONS: toc:2\n\n")

      (dolist (exp (sort experiments #'string<))
        (when (file-directory-p exp)
          (let ((readme (expand-file-name "README.org" exp)))
            (when (file-exists-p readme)
              (insert "\n* " (file-name-nondirectory exp) "\n")
              (insert-file-contents readme)
              (goto-char (point-max))
              (insert "\n"))))))
    (find-file output-file)))

;;; Running Experiments

(defun racketcon-run-current-experiment ()
  "Run the experiment in the current directory."
  (interactive)
  (let ((main-file (or (cl-find-if (lambda (f) (string-match-p "main\\.rkt$" f))
                                  (directory-files default-directory nil "\\.rkt$"))
                      (cl-find-if (lambda (f) (string-match-p "example\\.rkt$" f))
                                  (directory-files default-directory nil "\\.rkt$"))
                      (car (directory-files default-directory nil "\\.rkt$")))))
    (if main-file
        (racket-run (expand-file-name main-file default-directory))
      (message "No .rkt file found in current directory"))))

(defun racketcon-test-current-experiment ()
  "Run tests for current experiment."
  (interactive)
  (racket-test))

;;; Quick Insert Templates

(defun racketcon-insert-struct-lens (struct-name field)
  "Insert struct lens for STRUCT-NAME and FIELD."
  (interactive "sStruct name: \nsField name: ")
  (insert (format "(define %s-lens\n" field))
  (insert (format "  (lens\n"))
  (insert (format "    %s-%s\n" struct-name field))
  (insert (format "    (λ (s v) (struct-copy %s s [%s v]))))\n" struct-name field)))

(defun racketcon-insert-maybe ()
  "Insert Maybe type definition."
  (interactive)
  (insert "(struct some (value) #:transparent)\n")
  (insert "(struct none () #:transparent)\n")
  (insert "(define NONE (none))\n"))

;;; Keymap

(defvar racketcon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r l") #'racketcon-list-experiments)
    (define-key map (kbd "C-c r g") #'racketcon-goto-experiment)
    (define-key map (kbd "C-c r n") #'racketcon-new-experiment)
    (define-key map (kbd "C-c r r") #'racketcon-run-current-experiment)
    (define-key map (kbd "C-c r t") #'racketcon-test-current-experiment)

    ;; RHEA
    (define-key map (kbd "C-c r h") #'racketcon-rhea-new-hypothesis)
    (define-key map (kbd "C-c r e") #'racketcon-rhea-new-experiment)
    (define-key map (kbd "C-c r o") #'racketcon-rhea-export-org)

    ;; Optics
    (define-key map (kbd "C-c r i l") #'racketcon-insert-lens)
    (define-key map (kbd "C-c r i p") #'racketcon-insert-prism)
    (define-key map (kbd "C-c r i t") #'racketcon-insert-traversal)
    (define-key map (kbd "C-c r i v") #'racketcon-verify-lens-laws)

    ;; 2D
    (define-key map (kbd "C-c r 2") #'racketcon-insert-2d-grid)

    map)
  "Keymap for RacketCon tools.")

;;; Minor Mode

;;;###autoload
(define-minor-mode racketcon-mode
  "Minor mode for RacketCon 2025 experiment tools."
  :lighter " RCon"
  :keymap racketcon-mode-map
  :group 'racketcon)

;;;###autoload
(defun racketcon-mode-enable ()
  "Enable racketcon-mode if in RacketCon project."
  (when (and (buffer-file-name)
             (locate-dominating-file (buffer-file-name) ".dir-locals.el")
             (or (derived-mode-p 'racket-mode)
                 (derived-mode-p 'org-mode)))
    (racketcon-mode 1)))

;;;###autoload
(add-hook 'racket-mode-hook #'racketcon-mode-enable)
;;;###autoload
(add-hook 'org-mode-hook #'racketcon-mode-enable)

(provide 'racketcon-tools)

;;; racketcon-tools.el ends here
