# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `parseFloat`
- `narrowOrCrash`
- `isOrCrash` and `assertType`.
- Readme: Bad uses section.
- `min` and `max`
- `isFull`
- `orElseLazy`

### Changed
- Migrated from tslint to eslint.
- Improved documentation of `actToOpt`.

## [4.0.1] - 2022-05-18
### Changed
- Fixed `testReOrFalse` method crashing on `None`.

## [4.0.0] - 2022-05-12
### Added
- `altOpt`.
- `find`.
- Change log.

### Changed
- Implemented support for strings in `head`, `last` and `at`.
- `at`, `head` and `last` now support empty inputs.
- Improved return types of `opt` (and its variants), `at` and `chainToOpt`.

### Removed
- Deprecated `optOrCrash` (use `someOrCrash`).
- Deprecated `orElseOpt` (use `alt` instead).
