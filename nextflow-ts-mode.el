;;; nextflow-ts-mode.el --- Tree-sitter major mode for Nextflow files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Edmund Miller
;;
;; Author: Edmund Miller <http://github/emiller88>
;; Maintainer: Edmund Miller <Edmund.A.Miller@gmail.com>
;; Created: August 04, 2025
;; Version: 0.0.1
;; Keywords: tools nextflow bioinformatics languages
;; Homepage: https://github.com/emiller88/nextflow-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Major mode for editing Nextflow files using the built-in Emacs 29+
;; tree-sitter support (treesit.el) and the
;; https://github.com/nextflow-io/tree-sitter-nextflow grammar.
;;
;; To install the grammar:
;;
;;   M-x treesit-install-language-grammar RET nextflow RET
;;
;; This mode is the tree-sitter counterpart of `nextflow-mode', which
;; remains available as the regex-based fallback for Emacs < 29.
;;
;;; Code:

(require 'treesit)
(require 'cl-lib)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

;;; Customization

(defgroup nextflow-ts nil
  "Support for Nextflow files with tree-sitter."
  :group 'languages
  :prefix "nextflow-ts-")

(defcustom nextflow-ts-indent-offset 4
  "Number of spaces for each indentation step in `nextflow-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'nextflow-ts)

;;; Grammar source

;;;###autoload
(add-to-list 'treesit-language-source-alist
             '(nextflow "https://github.com/nextflow-io/tree-sitter-nextflow"))

;;; Font-lock

(defvar nextflow-ts-mode--keywords
  '("process" "workflow" "include" "from" "as" "def" "if" "else"
    "assert" "return" "new" "try" "catch" "finally" "exit"
    "in" "instanceof" "tuple" "template"
    "input:" "output:" "when:" "take:" "main:" "emit:"
    "script" "shell" "exec" "stub")
  "Nextflow keyword and section-label tokens for tree-sitter font-locking.")

(defvar nextflow-ts-mode--operators
  '("=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" "&=" "|=" "^=" "?="
    "+" "-" "*" "/" "%" "**"
    "==" "!=" "<" ">" "<=" ">="
    "&&" "||" "!"
    ".." "..<"
    "=~" "!~" "==~"
    "?:" "<=>" "<<" "&" "^" "~"
    "|" "->")
  "Nextflow operator tokens for tree-sitter font-locking.")

(defvar nextflow-ts-mode--builtin-regexp
  (regexp-opt '("baseDir" "launchDir" "moduleDir" "projectDir" "workDir"
                "workflow" "it" "task")
              'symbols)
  "Regexp matching Nextflow built-in variables.")

(defvar nextflow-ts-mode--type-regexp
  (regexp-opt '("val" "env" "eval" "file" "path" "stdin" "stdout" "each"
                "Channel" "Date" "Map" "List")
              'symbols)
  "Regexp matching Nextflow type-like qualifiers.")

(defvar nextflow-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'nextflow
   :feature 'comment
   '([(line_comment) (block_comment) (shebang)] @font-lock-comment-face)

   :language 'nextflow
   :feature 'string
   '([(string_literal)
      (triple_quoted_string)
      (slashy_string)
      (string)
      (interpolated_string)
      (interpolated_triple_quoted_string)]
     @font-lock-string-face)

   :language 'nextflow
   :feature 'keyword
   `([,@nextflow-ts-mode--keywords] @font-lock-keyword-face)

   :feature 'definition
   :language 'nextflow
   '((process_definition (identifier) @font-lock-function-name-face)
     (workflow_definition (identifier) @font-lock-function-name-face)
     ;; `def name(...)' -- no return_type field, name is first named child.
     (function_definition !return_type :anchor
                          (identifier) @font-lock-function-name-face)
     ;; `ReturnType name(...)' -- name immediately follows the return type.
     (function_definition return_type: (_) @font-lock-type-face :anchor
                          (identifier) @font-lock-function-name-face))

   :language 'nextflow
   :feature 'directive
   '((directive :anchor (identifier) @font-lock-preprocessor-face))

   :language 'nextflow
   :feature 'type
   `(((identifier) @font-lock-type-face
      (:match ,nextflow-ts-mode--type-regexp @font-lock-type-face)))

   :language 'nextflow
   :feature 'builtin
   `(["nextflow" "params" "env" "Channel"] @font-lock-builtin-face
     (process_output "out" @font-lock-builtin-face)
     (map_operation "map" @font-lock-builtin-face)
     ((identifier) @font-lock-builtin-face
      (:match ,nextflow-ts-mode--builtin-regexp @font-lock-builtin-face)))

   :language 'nextflow
   :feature 'constant
   `([(boolean_literal) (boolean)] @font-lock-constant-face
     ((identifier) @font-lock-constant-face
      (:match "\\`null\\'" @font-lock-constant-face)))

   :language 'nextflow
   :feature 'number
   '([(integer_literal) (float_literal) (number)] @font-lock-number-face)

   :language 'nextflow
   :feature 'variable
   '((parameter (identifier) @font-lock-variable-name-face)
     (variable_declaration (identifier) @font-lock-variable-name-face)
     (assignment (identifier) @font-lock-variable-name-face)
     (include_item (identifier) @font-lock-variable-name-face))

   :language 'nextflow
   :feature 'interpolation
   :override t
   '((interpolation) @font-lock-variable-name-face)

   :language 'nextflow
   :feature 'escape
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'nextflow
   :feature 'function
   '((function_call (identifier) @font-lock-function-call-face)
     (process_invocation (identifier) @font-lock-function-call-face)
     (command_expression (identifier) @font-lock-function-call-face))

   :language 'nextflow
   :feature 'operator
   `([,@nextflow-ts-mode--operators] @font-lock-operator-face)

   :language 'nextflow
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face))
  "Tree-sitter font-lock settings for `nextflow-ts-mode'.")

;;; Indentation

(defvar nextflow-ts-mode--indent-rules
  `((nextflow
     ((parent-is "source_file") column-0 0)
     ((node-is "}") standalone-parent 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "process_definition") standalone-parent nextflow-ts-indent-offset)
     ((parent-is "workflow_definition") standalone-parent nextflow-ts-indent-offset)
     ((parent-is "workflow_body") parent-bol nextflow-ts-indent-offset)
     ((parent-is "function_definition") standalone-parent nextflow-ts-indent-offset)
     ((parent-is "block") standalone-parent nextflow-ts-indent-offset)
     ((parent-is "closure") standalone-parent nextflow-ts-indent-offset)
     ((parent-is "list") parent-bol nextflow-ts-indent-offset)
     ((parent-is "map") parent-bol nextflow-ts-indent-offset)
     ((parent-is "input_declaration") parent-bol 0)
     ((parent-is "output_declaration") parent-bol 0)
     (catch-all parent-bol 0)))
  "Tree-sitter indent rules for `nextflow-ts-mode'.")

;;; Bash injection

(defvar nextflow-ts-mode--range-settings
  (when (treesit-available-p)
    (treesit-range-rules
     :embed 'bash
     :host 'nextflow
     ;; Capture only the string *content*, not the quote delimiters,
     ;; otherwise bash parses the surrounding quotes as a string.
     ;; string_literal / triple_quoted_string are single tokens with
     ;; no content child, so they are NOT injected: bash would receive
     ;; the quote delimiters and parse the body as one quoted word.
     '((script_content
        (interpolated_triple_quoted_string
         (triple_string_content) @capture))
       (script_content
        (interpolated_string
         (string_content) @capture)))))
  "Tree-sitter range rules injecting bash into process script blocks.")

(defun nextflow-ts-mode--language-at-point (point)
  "Return the treesit language at POINT: `bash' in injected script content."
  (let ((node (treesit-node-at point 'nextflow)))
    ;; Only interpolated script bodies are injected (must mirror
    ;; `nextflow-ts-mode--range-settings').
    (if (and node
             (member (treesit-node-type node)
                     '("triple_string_content" "string_content"))
             (treesit-parent-until
              node
              (lambda (n)
                (equal (treesit-node-type n) "script_content"))))
        'bash
      'nextflow)))

(defun nextflow-ts-mode--bash-font-lock-settings ()
  "Return bash tree-sitter font-lock settings from `sh-script'.
Used to fontify embedded bash in process script blocks.  The
settings are marked as overriding so they win over the host
string face."
  (require 'sh-script)
  (when (boundp 'sh-mode--treesit-settings)
    (mapcar (lambda (setting)
              (list (nth 0 setting) (nth 1 setting) (nth 2 setting) t))
            sh-mode--treesit-settings)))

;;; Navigation and imenu

(defun nextflow-ts-mode--defun-name (node)
  "Return the name of the process/workflow/function NODE, or nil."
  (when (member (treesit-node-type node)
                '("process_definition" "workflow_definition"
                  "function_definition"))
    (let ((child (treesit-node-child node 1)))
      (when (equal (treesit-node-type child) "identifier")
        (treesit-node-text child t)))))

;;; Mode

;;;###autoload
(define-derived-mode nextflow-ts-mode prog-mode "Nextflow[ts]"
  "Major mode for editing Nextflow files, using tree-sitter."
  :group 'nextflow-ts
  (when (treesit-ready-p 'nextflow)
    ;; Create the bash parser (used for script-block injection) first:
    ;; `treesit-parser-list' is newest-first and generic treesit
    ;; functions take the head as the buffer's main parser, so the
    ;; nextflow parser must be created last.
    (when (treesit-ready-p 'bash t)
      (treesit-parser-create 'bash))
    (treesit-parser-create 'nextflow)

    ;; Comments.
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx (or "//" "/*") (* (syntax whitespace))))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                nextflow-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string type directive)
                  (builtin constant number variable interpolation escape)
                  (function operator bracket)))

    ;; Indentation.
    (setq-local treesit-simple-indent-rules nextflow-ts-mode--indent-rules)

    ;; Bash injection into script blocks (requires the bash grammar).
    (when (treesit-ready-p 'bash t)
      (setq-local treesit-range-settings nextflow-ts-mode--range-settings)
      (setq-local treesit-language-at-point-function
                  #'nextflow-ts-mode--language-at-point)
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings
                          (nextflow-ts-mode--bash-font-lock-settings)))
      ;; Enable the bash features sh-script defines at their usual levels.
      (setq-local treesit-font-lock-feature-list
                  (cl-loop for ours in treesit-font-lock-feature-list
                           for extra in '((comment function)
                                          (command declaration-command keyword
                                           string string-interpolation heredoc)
                                          (builtin-variable constant number
                                           variable)
                                          (bracket delimiter misc-punctuation
                                           operator))
                           collect (delete-dups (append ours extra)))))
    (setq-local indent-tabs-mode nil)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (rx bos (or "process_definition"
                            "workflow_definition"
                            "function_definition")
                    eos))
    (setq-local treesit-defun-name-function #'nextflow-ts-mode--defun-name)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                '(("Process" "\\`process_definition\\'" nil nil)
                  ("Workflow" "\\`workflow_definition\\'" nil nil)
                  ("Function" "\\`function_definition\\'" nil nil)))

    (treesit-major-mode-setup)))

;;;###autoload
(when (treesit-ready-p 'nextflow t)
  (add-to-list 'major-mode-remap-alist
               '(nextflow-mode . nextflow-ts-mode)))

(provide 'nextflow-ts-mode)
;;; nextflow-ts-mode.el ends here
