---
purpose: Orient agents in the Emacs modes and their test/verification loop
applies_to: whole repository
entrypoint: nextflow-ts-mode.el (tree-sitter mode); nextflow-mode.el (legacy regex mode)
verification: emacs --batch -L . -l test/nextflow-ts-mode-test.el --eval "(ert-run-tests-batch-and-exit)"
update_when: mode files, range rules, or the test suite change
---
# nextflow-mode — agent guide

Two Emacs major modes for Nextflow:

- `nextflow-ts-mode.el` — tree-sitter based (Emacs 29+). Uses the
  `tree-sitter-nextflow` grammar plus an embedded bash parser for process
  `script:` bodies.
- `nextflow-mode.el` — legacy regex/font-lock mode. Keep changes minimal.

## Bash injection (the tricky part)

`nextflow-ts-mode--range-settings` and
`nextflow-ts-mode--language-at-point` MUST stay in sync: both list the
same content node types (`triple_string_content`, `string_content`) under
a `script_content` ancestor. Only *interpolated* string content is
injected — `string_literal`/`triple_quoted_string` are single tokens, so
injecting them would hand bash the quote delimiters and it would parse the
body as one quoted word.

This mirrors `queries/injections.scm` in the `tree-sitter-nextflow` repo
(the editor-agnostic injection spec:
https://tree-sitter.github.io/tree-sitter/3-syntax-highlighting.html#language-injection).
Grammar-side capture ranges are unit-tested there; **this repo's ERT suite
is the end-to-end fontification test** — `tree-sitter test` cannot verify
injection results, so don't move that coverage out of here.

## Testing

```bash
# full suite (12 tests; injection tests skip without nextflow+bash grammars)
emacs --batch -L . -l test/nextflow-ts-mode-test.el \
  --eval "(ert-run-tests-batch-and-exit)"

# byte-compile lint; remove the .elc afterward
emacs --batch -L . -f batch-byte-compile nextflow-ts-mode.el
```

Injection tests assert `font-lock-builtin-face` on `echo` inside script
bodies — that face can only come from the embedded bash parser. Guard new
grammar-dependent tests with quiet readiness checks:
`(skip-unless (and (treesit-ready-p 'nextflow t) (treesit-ready-p 'bash t)))`.
