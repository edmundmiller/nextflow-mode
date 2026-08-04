---
purpose: Record user-visible changes by nextflow-mode release
applies_to: nextflow-mode.el and nextflow-ts-mode.el releases
entrypoint: Unreleased for pending work; latest version for release notes
verification: package header versions and release links match
update_when: user-visible mode behavior changes or a release is prepared
---

# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2026-08-04

### Added

- `nextflow-ts-mode` for Emacs 29+, with Tree-sitter font locking,
  indentation, Imenu, and process/workflow/function navigation.
- Embedded Bash fontification inside interpolated single-line and
  triple-quoted process script bodies.
- ERT coverage for queries, font locking, indentation, Imenu, and both Bash
  injection forms, plus CI validation and agent testing guidance.

## [0.1.0] - 2024-12-26

### Added

- `nextflow-mode`, the Groovy-derived major mode for Nextflow syntax,
  indentation, evaluation, and nf-test files.

### Fixed

- Anonymous processes are recognized as anonymous workflows.
- Package metadata, dependencies, and customization groups satisfy Emacs
  package conventions.

[Unreleased]: https://github.com/edmundmiller/nextflow-mode/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/edmundmiller/nextflow-mode/compare/v0.1.0...v1.0.0
[0.1.0]: https://github.com/edmundmiller/nextflow-mode/releases/tag/v0.1.0
