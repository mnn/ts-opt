# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Added `altOpt`.
- Implemented `find`.
- Added change log.

### Changed
- Implemented support for strings in `head`, `last` and `at`.
- `at`, `head` and `last` now support empty inputs.
- Improved return types of `opt` (and its variants), `at` and `chainToOpt`.

### Removed
- Removed deprecated `optOrCrash` (use `someOrCrash`).
- Removed deprecated `orElseOpt` (use `alt` instead).
