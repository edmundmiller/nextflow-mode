;;; nextflow-mode.el --- Major mode for editing Nextflow files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Edmund Miller
;;
;; Author: Edmund Miller <http://github/emiller88>
;; Maintainer: Edmund Miller <Edmund.A.Miller@gmail.com>
;; Created: February 28, 2020
;; Modified: February 28, 2020
;; Version: 0.0.1
;; Keywords: tools nextflow bioinformatics
;; Homepage: https://github.com/emiller88/nextflow-mode
;; Package-Requires: ((emacs "24.5") (cl-lib "0.5") (magit-popup "2.4.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(require 'cl-lib)
(require 'groovy-mode)


;;; Customization

;;;###autoload
(defgroup nextflow-mode nil
  "Support for Nextflow files"
  :group 'tools)

(defcustom nextflow-mode-hook nil
  "Hook run when entering `nextflow-mode'."
  :type 'hook)

(defcustom nextflow-indent-field-offset 4
  "Offset for field indentation."
  :type 'integer)

(defcustom nextflow-indent-value-offset 4
  "Offset for field values that the line below the field key."
  :type 'integer)


;;; Regexp

(eval-and-compile
  (defconst nextflow-rx-constituents
    `((named-rule . ,(rx (and (group symbol-start
                                     (or "checkpoint" "rule" "subworkflow"))
                              " "
                              (group (one-or-more
                                      (or (syntax word) (syntax symbol)))))))
      (anon-rule . ,(rx symbol-start "rule"))
      (block-key . ,(rx symbol-start
                        (or "input"
                            "output"
                            "script"
                            "shell"
                            "exec")
                        symbol-end))
      (sm-command . ,(rx symbol-start
                         (or "configfile"
                             "include"
                             "localrules"
                             "onerror"
                             "onsuccess"
                             "report"
                             "ruleorder"
                             "singularity"
                             "wildcard_constraints"
                             "workdir")
                         symbol-end))
      (sm-builtin . ,(rx symbol-start
                         (or "ancient"
                             "checkpoints"
                             "directory"
                             "dynamic"
                             "expand"
                             "input"
                             "multiext"
                             "output"
                             "params"
                             "pipe"
                             "protected"
                             "report"
                             "shell"
                             "temp"
                             "touch"
                             "unpack"
                             "wildcards")
                         symbol-end))
      ;; Deprecated.  Use `sm-builtin' instead.
      (sm-func . sm-builtin))
    "Nextflow-specific sexps for `nextflow-rx'.")

  (defmacro nextflow-rx (&rest regexps)
    "Specialized `rx' for Nextflow mode."
    ;; Modified from `groovy-rx'.
    (let ((rx-constituents (append nextflow-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))

(defconst nextflow-rule-or-subworkflow-re
  (nextflow-rx line-start (zero-or-more space)
                (or named-rule (group anon-rule))
                (zero-or-more space) ":")
  "Regexp matching a rule or subworkflow.")


;;; Info and navigation

(defun nextflow-block-info ()
  "Return description of rule or subworkflow block at point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((pos (point)))
        (end-of-line)
        (and (re-search-backward nextflow-rule-or-subworkflow-re nil t)
             (let ((type (or (match-string-no-properties 1)
                             "rule"))
                   (name (match-string-no-properties 2))
                   (start (or (match-beginning 1)
                              (match-beginning 3)))
                   (rule-indent (current-indentation))
                   end)
               (beginning-of-line)
               (forward-line)
               (while (and (or (< rule-indent (current-indentation))
                               (looking-at-p "^\\s-*$"))
                           (or (not (eobp))
                               (progn (setq end (point-max))
                                      nil)))
                 (setq end (line-end-position))
                 (forward-line))
               (when (<= start pos end)
                 (list type name start end))))))))

(defun nextflow-beginning-of-block (&optional arg)
  "Move to beginning of rule block.
With ARG, do it that many times.  Negative ARG signals to move
forward rather than backward."
  (when (or (null arg) (zerop arg))
    (setq arg 1))
  (funcall (if (> arg 0)
               #'re-search-backward
             (lambda (&rest args)
               (end-of-line)
               (prog1 (apply #'re-search-forward args)
                 (beginning-of-line))))
           nextflow-rule-or-subworkflow-re
           nil 'move (abs arg))
  (looking-at-p nextflow-rule-or-subworkflow-re))

(defun nextflow-end-of-block ()
  "Move to end of rule or subworkflow block."
  (let ((end (nth 3 (nextflow-block-info))))
    (when end (goto-char end))))

(defun nextflow-beginning-of-defun (&optional arg)
  "Move to beginning of current rule block or function.
With ARG, do it that many times.  Negative ARG signals to move
forward rather than backward."
  (when (or (null arg) (zerop arg))
    (setq arg 1))
  (let ((choose-fn (if (> arg 0) #'max #'min))
        (cands (delq nil
                     (mapcar
                      (lambda (f)
                        (save-excursion (and (funcall f arg) (point))))
                      (list #'nextflow-beginning-of-block
                            #'groovy-nav-beginning-of-defun)))))
    (cond (cands
           (goto-char (apply choose-fn cands)))
          ((> arg 0)
           (goto-char (point-min))
           nil)
          (t
           (goto-char (point-max))
           nil))))

(defun nextflow-end-of-defun ()
  "Move to end of current rule block or function."
  (or (nextflow-end-of-block)
      (groovy-nav-end-of-defun)))

(defun nextflow-block-or-defun-name ()
  "Return name of current rule or function.
This function is appropriate to use as the value of
`add-log-current-defun-function'."
  (or (nth 1 (nextflow-block-info))
      (groovy-info-current-defun)))


;;; Indentation

(defun nextflow--calculate-indentation (&optional previous)
  "Return indentation offset for the current line.

A non-nil value for PREVIOUS indicates that the previous command
was an indentation command.

When Groovy mode should handle the indentation, a nil value is
returned."
  (when (memq (car (groovy-indent-context))
              (list :after-line
                    ;; If point is on a value line following a naked
                    ;; field value, `groovy-indent-context' returns
                    ;; :after-block-start.
                    :after-block-start))
    (let* ((initial-indent (current-indentation))
           (goto-first-p (or (not previous) (zerop initial-indent))))
      (save-excursion
        (save-restriction
          (widen)
          (beginning-of-line)
          (if (looking-at-p (nextflow-rx
                             line-start (zero-or-more space)
                             (or (and field-key (zero-or-more space) ":")
                                 (or "\"\"\"" "'''"))))
              (and goto-first-p
                   (let (rule-indent)
                     (while (not (or rule-indent (bobp)))
                       (forward-line -1)
                       (when (looking-at-p nextflow-rule-or-subworkflow-re)
                         (setq rule-indent (current-indentation))))
                     (and rule-indent
                          (+ rule-indent nextflow-indent-field-offset))))
            ;; We need to look back to determine indentation.
            (skip-chars-backward " \t\n")
            (beginning-of-line)
            (cond
             ((looking-at-p (nextflow-rx
                             line-start (zero-or-more space)
                             (or named-rule anon-rule
                                 field-key sm-command)
                             (zero-or-more space) ":" (zero-or-more space)
                             (zero-or-one (and "#" (zero-or-more not-newline)))
                             line-end))
              (let ((above-indent (current-indentation)))
                (cond (goto-first-p
                       (+ above-indent nextflow-indent-value-offset))
                      ((< above-indent initial-indent)
                       above-indent))))
             ((looking-at (nextflow-rx
                           line-start (zero-or-more space)
                           field-key
                           (zero-or-more space) ":" (zero-or-more space)))
              (let ((above-indent (current-indentation)))
                (cond (goto-first-p
                       (- (match-end 0) (line-beginning-position)))
                      ((< above-indent initial-indent)
                       above-indent))))
             ((save-excursion
                (let ((above-indent (current-indentation))
                      field-indent)
                  (when (> above-indent 0)
                    (while (and (not (bobp))
                                (or (= above-indent
                                       (setq field-indent (current-indentation)))
                                    (looking-at-p "^\\s-*$")))
                      (forward-line -1)))
                  (and (looking-at (nextflow-rx
                                    line-start (zero-or-more space)
                                    (group field-key)
                                    (zero-or-more space) ":"
                                    (zero-or-more space)
                                    (group (zero-or-more not-newline))))
                       (not (equal (match-string-no-properties 1)
                                   "run"))
                       (cond (goto-first-p
                              (let ((field-val (match-string-no-properties 2)))
                                (if (or (equal field-val "")
                                        (string-match-p "\\`#" field-val))
                                    above-indent
                                  (- (match-beginning 2) (line-beginning-position)))))
                             ((< field-indent initial-indent)
                              field-indent)))))))))))))

(defun nextflow-indent-line (&optional previous)
  "Nextflow mode variant of `groovy-indent-line'."
  (let ((follow-indentation-p
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (or (nextflow--calculate-indentation previous)
           (groovy-indent-calculate-indentation previous))))
    (when follow-indentation-p
      (back-to-indentation))))

(defun nextflow-indent-line-function ()
  "Nextflow mode variant of `groovy-indent-line-function'."
  (nextflow-indent-line
   (and (memq this-command groovy-indent-trigger-commands)
        (eq last-command this-command))))


;;; Imenu

(defun nextflow-imenu-create-index ()
  "Create Imenu index for rule blocks.
If `groovy-imenu-create-index' returns a non-nil value, also
include these results and append a \"(rule)\" to the index
label."
  (let ((-index (groovy-imenu-create-index))
        (sm-index (nextflow--imenu-build-rule-index)))
    (if groovy-index
        (append (mapcar (lambda (x)
                          (cons (concat (car x) " (rule)") (cdr x)))
                        sm-index)
                groovy-index)
      sm-index)))

(defun nextflow--imenu-build-rule-index ()
  (goto-char (point-min))
  (let (index)
    (while (re-search-forward nextflow-rule-or-subworkflow-re nil t)
      (push (cons (match-string-no-properties 2)
                  (save-excursion (beginning-of-line)
                                  (point-marker)))
            index))
    (nreverse index)))


;;; Embedded language syntax-highlighting

(declare-function mmm-add-classes "mmm-vars")
(declare-function mmm-add-mode-ext-class "mmm-vars")

(defun nextflow-mode-setup-mmm ()
  "Set up MMM mode to highlight embedded R code.

You must have the R-strings either within a R(\"\"\" \"\"\") function
call or a code block delimited with \"\"\"#r and \"\"\".  (Triple
single-quotes also accepted.)

For automatic highlighting of embedded regions, you need to set
`mmm-global-mode' to `maybe'."
  (unless (require 'mmm-mode nil t)
    (user-error "You need to install mmm-mode"))

  (when (unless (bound-and-true-p mmm-global-mode))
    (display-warning 'nextflow-mode "To get automatic syntax highlighting of
embedded R, you need to set mmm-global-mode to a non-nil value such as 'maybe."))

  (mmm-add-classes
   '((nextflow-R-call-double
      :submode R-mode
      :front ".*R\(\"\"\""
      :back ".*\"\"\"\)")))

  (mmm-add-classes
   '((nextflow-R-call-regular
      :submode R-mode
      :front ".*R\('''"
      :back ".*'''\)")))

  (mmm-add-classes
   '((nextflow-R-string-double
      :submode R-mode
      :front ".*\"\"\" *# *[rR]"
      :back ".*\"\"\"")))

  (mmm-add-classes
   '((nextflow-R-string-regular
      :submode R-mode
      :front ".*''' *# *[rR]"
      :back ".*'''")))

  (mmm-add-mode-ext-class 'nextflow-mode nil 'nextflow-R-call-double)
  (mmm-add-mode-ext-class 'nextflow-mode nil 'nextflow-R-call-regular)
  (mmm-add-mode-ext-class 'nextflow-mode nil 'nextflow-R-string-double)
  (mmm-add-mode-ext-class 'nextflow-mode nil 'nextflow-R-string-regular))


;;; Mode

(defvar nextflow--font-lock-keywords
    `((,nextflow-rule-or-subworkflow-re
       (1 font-lock-keyword-face nil 'lax)
       (2 font-lock-function-name-face nil 'lax)
       (3 font-lock-keyword-face nil 'lax))
      (,(nextflow-rx line-start (one-or-more space)
                      (group field-key)
                      (zero-or-more space) ":")
       1 font-lock-type-face)
      (,(nextflow-rx line-start (zero-or-more space)
                      (group sm-command)
                      (zero-or-more space) ":")
       1 font-lock-keyword-face)
      (,(nextflow-rx (group sm-builtin)) 1 font-lock-builtin-face)))

(if (bound-and-true-p groovy-font-lock-keywords-level-1)
    (with-no-warnings
      ;; In Emacs 27 `groovy-font-lock-keywords' was split up into
      ;; different decoration levels.
      (defvar nextflow-font-lock-keywords-level-1
        (append nextflow--font-lock-keywords
                groovy-font-lock-keywords-level-1))
      (defvar nextflow-font-lock-keywords-level-2
        (append nextflow--font-lock-keywords
                groovy-font-lock-keywords-level-2))
      (defvar nextflow-font-lock-keywords-maximum-decoration
        (append nextflow--font-lock-keywords
                groovy-font-lock-keywords-maximum-decoration))
      (defvar nextflow-font-lock-keywords
        ;; Mirrors `groovy-font-lock-keywords'.
        '(nextflow-font-lock-keywords-level-1
          nextflow-font-lock-keywords-level-1
          nextflow-font-lock-keywords-level-2
          nextflow-font-lock-keywords-maximum-decoration)))
  (defvar nextflow-font-lock-keywords
    (append nextflow--font-lock-keywords groovy-font-lock-keywords)))

;;;###autoload
(define-derived-mode nextflow-mode groovy-mode "Nextflow"
  "Mode for editing Nextflow files."
  (set (make-local-variable 'imenu-create-index-function)
       #'nextflow-imenu-create-index)
  (set (make-local-variable 'indent-line-function) 'nextflow-indent-line-function)
  (set (make-local-variable 'indent-region-function) nil)

  (set (make-local-variable 'beginning-of-defun-function)
       #'nextflow-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'nextflow-end-of-defun)
  (set (make-local-variable 'add-log-current-defun-function)
       #'nextflow-block-or-defun-name)

  (set (make-local-variable 'font-lock-defaults)
       (cons nextflow-font-lock-keywords (cdr font-lock-defaults))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:nf\\)?patterns\\'" . nextflow-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nf\\'" . nextflow-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nextflow\\'" . nextflow-mode))

(provide 'nextflow-mode)
;;; nextflow-mode.el ends here
