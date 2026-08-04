;;; nextflow-ts-mode-test.el --- Tests for nextflow-ts-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for nextflow-ts-mode.  Font-lock and indentation tests
;; require the nextflow tree-sitter grammar; they are skipped when it
;; is unavailable.  Run with:
;;   emacs -Q --batch -l ert -l nextflow-ts-mode.el \
;;     -l test/nextflow-ts-mode-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'treesit)
(require 'nextflow-ts-mode)

(defconst nextflow-ts-test--fixture
  (expand-file-name "fixtures/sample.nf"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to the sample Nextflow fixture.")

(defmacro nextflow-ts-test--with-fixture (&rest body)
  "Run BODY in a fontified buffer visiting the sample fixture."
  `(progn
     (skip-unless (treesit-ready-p 'nextflow))
     (with-temp-buffer
       (insert-file-contents nextflow-ts-test--fixture)
       (nextflow-ts-mode)
       (font-lock-ensure)
       ,@body)))

(defun nextflow-ts-test--face-at (needle)
  "Return the face at the start of the first occurrence of NEEDLE."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (match-beginning 0) 'face))

;;; Query validation (does not require a parse, only the grammar)

(ert-deftest nextflow-ts-mode-queries-valid ()
  "All font-lock queries compile against the installed grammar."
  (skip-unless (treesit-ready-p 'nextflow))
  (dolist (setting nextflow-ts-mode--font-lock-settings)
    (should (treesit-query-p (car setting)))))

;;; Font-lock

(ert-deftest nextflow-ts-mode-fontify-comment ()
  (nextflow-ts-test--with-fixture
   (should (eq (nextflow-ts-test--face-at "// A sample")
               'font-lock-comment-face))))

(ert-deftest nextflow-ts-mode-fontify-keywords ()
  (nextflow-ts-test--with-fixture
   (should (eq (nextflow-ts-test--face-at "process ") 'font-lock-keyword-face))
   (should (eq (nextflow-ts-test--face-at "workflow {") 'font-lock-keyword-face))
   (should (eq (nextflow-ts-test--face-at "include") 'font-lock-keyword-face))
   (should (eq (nextflow-ts-test--face-at "input:") 'font-lock-keyword-face))
   (should (eq (nextflow-ts-test--face-at "script:") 'font-lock-keyword-face))
   (should (eq (nextflow-ts-test--face-at "main:") 'font-lock-keyword-face))))

(ert-deftest nextflow-ts-mode-fontify-definition ()
  (nextflow-ts-test--with-fixture
   (should (eq (nextflow-ts-test--face-at "SAY_HELLO {")
               'font-lock-function-name-face))
   (should (eq (nextflow-ts-test--face-at "plain_helper")
               'font-lock-function-name-face))
   ;; Typed definition: name gets the function face, return type does not.
   (should (eq (nextflow-ts-test--face-at "typed_helper")
               'font-lock-function-name-face))
   (should-not (eq (nextflow-ts-test--face-at "String typed_helper")
                   'font-lock-function-name-face))))

(ert-deftest nextflow-ts-mode-fontify-directive ()
  (nextflow-ts-test--with-fixture
   (should (eq (nextflow-ts-test--face-at "tag \"") 'font-lock-preprocessor-face))
   (should (eq (nextflow-ts-test--face-at "cpus 2") 'font-lock-preprocessor-face))))

(ert-deftest nextflow-ts-mode-fontify-type ()
  (nextflow-ts-test--with-fixture
   (should (eq (nextflow-ts-test--face-at "val x") 'font-lock-type-face))))

(ert-deftest nextflow-ts-mode-fontify-builtin ()
  (nextflow-ts-test--with-fixture
   (should (eq (nextflow-ts-test--face-at "params.input =")
               'font-lock-builtin-face))
   (should (eq (nextflow-ts-test--face-at "Channel") 'font-lock-builtin-face))))

(ert-deftest nextflow-ts-mode-fontify-string ()
  (nextflow-ts-test--with-fixture
   (should (eq (nextflow-ts-test--face-at "\"\"\"") 'font-lock-string-face))
   ;; With the bash grammar installed, script content is fontified as
   ;; bash (echo is a builtin); otherwise it stays a plain string.
   (should (memq (nextflow-ts-test--face-at "echo Hello")
                 '(font-lock-string-face font-lock-builtin-face)))))

(ert-deftest nextflow-ts-mode-bash-injection ()
  "Script block content is fontified via the embedded bash parser."
  (skip-unless (and (treesit-ready-p 'nextflow) (treesit-ready-p 'bash)))
  (nextflow-ts-test--with-fixture
   ;; `treesit-parser-list' accepts only 0-1 args in Emacs 29.
   (should (memq 'bash (mapcar #'treesit-parser-language
                               (treesit-parser-list))))
   (should (eq (nextflow-ts-test--face-at "echo Hello")
               'font-lock-builtin-face))))

(ert-deftest nextflow-ts-mode-bash-injection-double-quoted ()
  "Double-quoted script bodies are fontified via the embedded bash parser."
  (skip-unless (and (treesit-ready-p 'nextflow) (treesit-ready-p 'bash)))
  (with-temp-buffer
    (insert "process FOO {\n"
            "    script:\n"
            "    \"echo hello\"\n"
            "}\n")
    (nextflow-ts-mode)
    (font-lock-ensure)
    ;; `treesit-parser-list' accepts only 0-1 args in Emacs 29.
    (should (memq 'bash (mapcar #'treesit-parser-language
                                (treesit-parser-list))))
    (should (eq (nextflow-ts-test--face-at "echo hello")
                'font-lock-builtin-face))))

;;; Indentation

(ert-deftest nextflow-ts-mode-indent-process ()
  "Re-indenting a process body reproduces the expected layout."
  (skip-unless (treesit-ready-p 'nextflow))
  (with-temp-buffer
    (insert "process FOO {\n"
            "cpus 2\n"
            "input:\n"
            "val x\n"
            "script:\n"
            "\"echo $x\"\n"
            "}\n")
    (nextflow-ts-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   (concat "process FOO {\n"
                           "    cpus 2\n"
                           "    input:\n"
                           "    val x\n"
                           "    script:\n"
                           "    \"echo $x\"\n"
                           "}\n")))))

;;; Imenu

(ert-deftest nextflow-ts-mode-imenu ()
  (nextflow-ts-test--with-fixture
   (let ((index (treesit-simple-imenu)))
     (should (assoc "SAY_HELLO" (cdr (assoc "Process" index))))
     (should (assoc "Workflow" index))
     ;; Typed function is indexed by its name, not its return type.
     (should (assoc "plain_helper" (cdr (assoc "Function" index))))
     (should (assoc "typed_helper" (cdr (assoc "Function" index))))
     (should-not (assoc "String" (cdr (assoc "Function" index)))))))

(provide 'nextflow-ts-mode-test)
;;; nextflow-ts-mode-test.el ends here
