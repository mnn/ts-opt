# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `join` method
- functions `not`, `and`, `or`, `xor`, `bool`, `inc`, `dec`, `crash`, `eq`, `eqAny`, `noop`
- better (de)serialization: function/methods `serialize`, `deserialize`, `deserializeOrCrash` and `deserializeUnsafe`
- implemented Jest snapshot serializer

### Changed
- improved return type of `actToOpt` method and function

## [5.0.0] - 2023-05-16
### Added
- `toObject` and `fromObject`
- `min2Num`, `min2All`, `min2Any`, `max2Num`, `max2All`, `max2Any` and `clamp`
- `filterByRe`
- `propOrCrash` and `genNakedPropOrCrash`

### Changed
- **Breaking**: `min` and `max` now accept naked and read-only arrays - it *may* lead to worse type inference in more complex use cases
- **Breaking**: Added support for readonly arrays to all array functions.

## [4.1.0] - 2022-09-14
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
