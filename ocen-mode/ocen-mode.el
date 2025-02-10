;; ocen-mode.el -- A major mode for the Ocen programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Freddie Firouzi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; ocen-mode.el --- Major mode for editing Ocen files


(defconst ocen-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Define operators and punctuation
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; Define comments
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Define strings and char
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Underscores are part of words
    (modify-syntax-entry ?_ "w" table)
    table))

(defconst ocen-keywords
  '("let" "const" "def" "if" "else" "match"
    "for" "while" "namespace" "import" "enum"
    "struct" "union" "return" "yield")
  "Ocen language keywords.")

(defconst ocen-builtin-types
  '("u8" "i8" "u16" "i16" "u32" "i32" "u64" "i64"
    "f32" "f64" "char" "str" "untyped_ptr" "bool")
  "Ocen built-in types.")

(defvar ocen-font-lock-keywords
  `((,(regexp-opt ocen-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt ocen-builtin-types 'symbols) . font-lock-type-face)
    ("\\<def\\s-+\\([A-Za-z0-9_:]+\\)" 1 font-lock-function-name-face)
    ;; String literals
    ("\"\\(?:\\\\.\\|[^\"]\\)*\"" . font-lock-string-face)
    ;; Backtick-based format strings
    ("`\\(?:\\\\.\\|[^`]\\)*`" . font-lock-string-face)
    ;; f-string format (Python-style)
    ("f\"\\(?:\\\\.\\|[^\"\\]\\)*\"" . font-lock-string-face)))

;;;###autoload
(define-derived-mode ocen-mode prog-mode "Ocen"
  "Major mode for editing Ocen files."
  :syntax-table ocen-mode-syntax-table
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s *")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local buffer-file-coding-system 'utf-8-unix)
  (setq-local electric-indent-chars (append "{}():;," electric-indent-chars))
  (setq-local indent-line-function #'js-indent-line)
  (setq-local js-indent-level tab-width)

  ;; Set up syntax highlighting
  (setq-local font-lock-defaults '((ocen-font-lock-keywords)))

  ;; Set up Imenu
  (setq-local imenu-generic-expression
              '(("Section" "^\\s-*// Section: \\(.*\\)" 1)
                ("Enum" "^enum \\([A-Za-z0-9_]+\\)" 1)
                ("Struct" "^struct \\([A-Za-z0-9_]+\\)" 1)
                ("Union" "^union \\([A-Za-z0-9_]+\\)" 1)
                ("Function" "^def \\([A-Za-z0-9_:]+\\)" 1)))
  (imenu-add-to-menubar "Index"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.oc\\'" . ocen-mode))

(provide 'ocen-mode)

;;; ocen-mode.el ends here
