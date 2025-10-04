;;; ion-mode.el --- Major mode for Amazon Ion data format  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 RacketCon 2025

;; Author: RacketCon 2025
;; Keywords: data, languages
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.1.0

;;; Commentary:

;; Major mode for editing Amazon Ion data format files.
;; Provides syntax highlighting for:
;;   - Ion types (null, bool, int, decimal, float, timestamp, etc.)
;;   - Annotations (SYMBOL::VALUE pattern)
;;   - Strings and symbols
;;   - S-expressions
;;   - Collections (lists, structs, sexps)
;;   - Comments (// and /* */)

;;; Code:

(defgroup ion nil
  "Major mode for Amazon Ion data format."
  :group 'languages
  :prefix "ion-")

(defcustom ion-indent-offset 2
  "Indentation offset for Ion code."
  :type 'integer
  :group 'ion)

;;; Syntax highlighting

(defconst ion-font-lock-keywords
  (list
   ;; Typed nulls
   '("\\<null\\(?:\\.\\(?:bool\\|int\\|decimal\\|float\\|timestamp\\|string\\|symbol\\|blob\\|clob\\|list\\|sexp\\|struct\\)\\)?\\>"
     . font-lock-constant-face)

   ;; Booleans
   '("\\<\\(?:true\\|false\\)\\>" . font-lock-constant-face)

   ;; Special float values
   '("\\<\\(?:[+-]?inf\\|nan\\)\\>" . font-lock-constant-face)

   ;; Annotations (SYMBOL:: pattern)
   '("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)::" 1 font-lock-type-face)

   ;; Timestamps
   '("\\<[0-9]\\{4\\}-[0-9]\\{2\\}\\(?:-[0-9]\\{2\\}\\)?T\\(?:[0-9:]\\|\\.[0-9]\\|[+-][0-9:]\\|Z\\)*"
     . font-lock-constant-face)

   ;; Hexadecimal integers
   '("\\<0x[0-9a-fA-F_]+\\>" . font-lock-constant-face)

   ;; Binary integers
   '("\\<0b[01_]+\\>" . font-lock-constant-face)

   ;; Decimal and float numbers
   '("\\<[+-]?[0-9][0-9_]*\\(?:\\.[0-9_]+\\)?\\(?:[deDE][+-]?[0-9]+\\)?\\>"
     . font-lock-constant-face)

   ;; Strings (double-quoted)
   '("\"\\(?:[^\"\\]\\|\\\\.\\)*\"" . font-lock-string-face)

   ;; Long strings (triple-quoted)
   '("'''\\(?:[^']\\|'[^']\\|''[^']\\)*'''" . font-lock-string-face)

   ;; Quoted symbols
   '("'\\([a-zA-Z_][a-zA-Z0-9_]*\\|[^']+\\)'" . font-lock-variable-name-face)

   ;; Unquoted symbols (in struct keys or standalone)
   '("\\<[a-zA-Z_][a-zA-Z0-9_]*\\>:" . font-lock-variable-name-face)

   ;; Blob delimiters
   '("{{\\|}}" . font-lock-keyword-face)

   ;; S-expression functions (common Lisp/Scheme keywords)
   '("(\\s-*\\(\\(?:define\\|lambda\\|let\\|if\\|cond\\|map\\|filter\\|fold[rl]?\\|compose\\|cons\\|car\\|cdr\\|list\\|quote\\|and\\|or\\|not\\|eq\\?\\|equal\\?\\|null\\?\\|zero\\?\\|add1\\|sub1\\|for-each\\|range\\|get\\|has-field\\?\\|defmacro\\|pipeline\\)\\)\\>"
     1 font-lock-keyword-face)
   )
  "Font lock keywords for Ion mode.")

;;; Syntax table

(defvar ion-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C++-style comments
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)

    ;; Brackets and braces
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)

    ;; Symbols
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?: "." st)

    st)
  "Syntax table for Ion mode.")

;;; Indentation

(defun ion-indent-line ()
  "Indent current line as Ion code."
  (interactive)
  (let ((indent-level 0)
        (offset ion-indent-offset))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent-level 0)
        ;; Count nesting level
        (let ((nest-level 0))
          (save-excursion
            (while (re-search-backward "[][{}()]" nil t)
              (cond
               ((looking-at "[])}]")
                (setq nest-level (1- nest-level)))
               ((looking-at "[\\[{(]")
                (setq nest-level (1+ nest-level))))))
          (setq indent-level (* offset (max 0 nest-level)))

          ;; Adjust for closing brackets on current line
          (when (looking-at "\\s-*[])}]")
            (setq indent-level (max 0 (- indent-level offset)))))))

    ;; Apply indentation
    (if (looking-at "\\s-*")
        (replace-match (make-string indent-level ?\s))
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to indent-level)))))

;;; Mode definition

;;;###autoload
(define-derived-mode ion-mode prog-mode "Ion"
  "Major mode for editing Amazon Ion data format files.

Ion is a richly-typed, self-describing data serialization format
developed by Amazon. It supports both text and binary encodings.

Key features:
  - Rich type system (timestamps, decimals, symbols, etc.)
  - Annotations for semantic metadata
  - S-expressions as first-class data
  - Both human-readable text and efficient binary formats

\\{ion-mode-map}"
  :syntax-table ion-mode-syntax-table

  ;; Set up font-lock
  (setq font-lock-defaults '(ion-font-lock-keywords))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Indentation
  (setq-local indent-line-function 'ion-indent-line)

  ;; Electric pairs
  (setq-local electric-pair-pairs
              '((?\" . ?\")
                (?\' . ?\')
                (?\[ . ?\])
                (?\{ . ?\})
                (?\( . ?\))))
  (setq-local electric-pair-text-pairs electric-pair-pairs)
  (electric-pair-local-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ion\\'" . ion-mode))

;;; Commands

(defun ion-format-buffer ()
  "Format the current Ion buffer (placeholder for future implementation)."
  (interactive)
  (message "Ion formatting not yet implemented"))

(defun ion-validate-buffer ()
  "Validate Ion syntax in current buffer (placeholder for future implementation)."
  (interactive)
  (message "Ion validation not yet implemented"))

;;; Integration with RacketCon tools

(defun ion-open-documentation ()
  "Open Ion documentation in browser."
  (interactive)
  (browse-url "https://amazon-ion.github.io/ion-docs/"))

(defun ion-open-grammar ()
  "Open Ion grammar specification in browser."
  (interactive)
  (browse-url "https://amazon-ion.github.io/ion-docs/books/ion-1-1/grammar.html"))

(defun ion-jump-to-examples ()
  "Jump to Ion examples file in RacketCon project."
  (interactive)
  (let ((examples-file (expand-file-name
                        "experiments/061-ion-data-format/examples.ion"
                        (locate-dominating-file default-directory ".git"))))
    (if (file-exists-p examples-file)
        (find-file examples-file)
      (message "Ion examples file not found: %s" examples-file))))

(defun ion-jump-to-tutorial ()
  "Jump to Ion tutorial in RacketCon project."
  (interactive)
  (let ((tutorial-file (expand-file-name
                        "experiments/061-ion-data-format/TUTORIAL.org"
                        (locate-dominating-file default-directory ".git"))))
    (if (file-exists-p tutorial-file)
        (find-file tutorial-file)
      (message "Ion tutorial not found: %s" tutorial-file))))

;;; Keybindings

(defvar ion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'ion-format-buffer)
    (define-key map (kbd "C-c C-v") 'ion-validate-buffer)
    (define-key map (kbd "C-c C-d") 'ion-open-documentation)
    (define-key map (kbd "C-c C-g") 'ion-open-grammar)
    (define-key map (kbd "C-c C-e") 'ion-jump-to-examples)
    (define-key map (kbd "C-c C-t") 'ion-jump-to-tutorial)
    map)
  "Keymap for Ion mode.")

(provide 'ion-mode)

;;; ion-mode.el ends here
