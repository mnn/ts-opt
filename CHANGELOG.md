# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `propOrNull`, `propOrNullNaked`, `propOrUndef`, `propOrUndefNaked`, `propOrZero` and `propOrZeroNaked`
- `genNakedPropGetters` and `genPropGetters`

### Changed
- `orCrash` method now also accepts an error factory function

## [6.0.2] - 2024-09-12
### Fixed
- `genNakedPropOrCrash` now correctly infers the type of the property

### Changed
- Improved examples of several methods and functions

## [6.0.1] - 2024-09-02
### Fixed
- Removed `@internal` from `OptSafe`

## [6.0.0] - 2024-09-02
### Added
- `elemOf`, `elemOfStr` and `elemOfStrIn`
- `mapPropNakedIn` function
- `optInfinity`, `mapStr` functions
- Various `*In` methods and functions: `flatMapIn`, `containsIn`, `has`, `hasIn`, `lengthIn`, `zipIn`, `findIn`, `filterIn`, `fold`, `foldIn`, `mapIn`

### Changed
- **Breaking**: Updated several functions and methods, like `head`, `last`, `min`, and `max`, to clearly differentiate between operations on arrays and opts.
- Swapped husky with simple-git-hooks
- Updated eslint config
- Updated most dependencies
- Moved to pnpm package manager
- Updated GitLab pipelines to use current Node.js version and pnpm
- Documentation updates and improvements

## [5.1.0] - 2023-11-08
### Added
- `join` method
- utility functions `not`, `and`, `or`, `xor`, `bool`, `inc`, `dec`, `crash`, `eq`, `eqAny`, `noop`, `appendStr`, `prependStr`
- better (de)serialization: function/methods `serialize`, `deserialize`, `deserializeOrCrash` and `deserializeUnsafe`
- implemented Jest snapshot serializer
- `optArrayOpt`, `noneIfEmpty` and `propNaked`

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
