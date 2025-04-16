# Changelog for `MSI`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.0.1] - 08.12.2022

Created project.

## [0.0.2] - 11.12.2022

### Added


- `AST` module with `AST` data type and `show` instance.
  - Base types: `uint8`, `bool`, `string`, `mapping`, `array`.
  - Binary operators: `+`, `-`, `*`, `/`, `==`, `!=`, `&&`, `||`, `!`, `>`, `<`, `>=`, `<=`.
  - Unary operators: `++`, `--`, `-`, `!`.
  - Expressions: var call, value expression, unary operation, binary operation, array/mapping/string element access, function call.
- Value operations: `+`, `-`, `*`, `/`, `==`, `!=`, `&&`, `||`, `!`, `>`, `<`, `>=`, `<=`.
