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
    `((nf-type . ,(rx (and (group symbol-start
                             (or "process" "file" "val" "Channel"))
                        " "
                        (group (one-or-more
                                 (or (syntax word) (syntax symbol)))))))
       (anon-process . ,(rx symbol-start "process"))
       (nf-directive . ,(rx symbol-start
                          (or "afterScript"
                            "beforeScript"
                            "cache"
                            "container"
                            "cpus"
                            "clusterOptions"
                            "disk"
                            "echo"
                            "errorStrategy"
                            "executor"
                            "ext"
                            "label"
                            "maxErrors"
                            "maxForks"
                            "maxRetries"
                            "memory"
                            "module"
                            "penv"
                            "publishDir"
                            "queue"
                            "scratch"
                            "storeDir"
                            "stageInMode"
                            "stageOutMode"
                            "tag"
                            "time"
                            "validExitStatus")
                          symbol-end))
       (nf-block . ,(rx symbol-start
                      (or "input"
                        "output"
                        "script"
                        "shell"
                        "exec")
                      symbol-end))
       (nf-keyword . ,(rx symbol-start
                        (or "from"
                          "into")
                        symbol-end))
       (nf-special . ,(rx symbol-start
                        (or "workflow"
                          "params"
                          "launchDir")
                        symbol-end))
       (nf-constant . ,(rx symbol-start
                         (or "null")
                         symbol-end)))
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

(defconst nextflow-process-re
  (nextflow-rx line-start (zero-or-more space)
    (or nf-type (group anon-process)))
  "Regexp matching a rule or subworkflow.")

;;; Mode

(defvar nextflow--font-lock-keywords
  `((,nextflow-process-re
      (1 font-lock-keyword-face nil 'lax)
      (2 font-lock-function-name-face nil 'lax)
      (3 font-lock-keyword-face nil 'lax)
      (,(nextflow-rx line-start (one-or-more space)
          (group nf-block)
          (zero-or-more space) ":")
        1 font-lock-function-name-face)
      (,(nextflow-rx line-start (zero-or-more space)
          (group nf-directive)
          (one-or-more space) "'")
        1 font-lock-keyword-face)
      (,(nextflow-rx (group nf-type)) 1 font-lock-type-face))))

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
