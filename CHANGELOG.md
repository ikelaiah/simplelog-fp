# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Release [0.8.0] - 2026-05-18

### Added

- Added a short `docs/getting-started.md` guide focused on the common first-use workflow.

### Changed

- Simplified the public API by making formatted logging available through level overloads only.
- Refined test helpers and behavior tests around formatted logging so they match the public API new users see.
- Updated README and user manual references for the v0.8.0 developer-experience pass.

### Removed

- Removed public `LogFmt`; formatted logging remains available through `Debug`, `Info`, `Warning`, `Error`, and `Fatal` overloads.

## Release [0.7.0] - 2026-05-18

### Added

- `SetUseColors` and `UseColors` to allow plain console output without color control codes.

### Changed

- Configuration is now method-first: public configuration properties are read-only and should be changed through `Set...` methods.
- Factory initialization now uses one shared internal helper for consistent defaults.
- File rotation now keeps one bounded backup (`app.log.1`) instead of timestamped backups.
- Logging calls and individual configuration methods are serialized with the same internal lock.
- Documentation now describes thread safety more precisely and recommends configuring loggers before sharing them across threads.
- Expanded FPCUnit coverage for `LogFmt`, bounded rotation replacement, minimum file-size clamping, log format shape, invalid file targets, and color opt-out.

### Removed

- Removed the no-op `Finalize` method from the public API.

## Release [0.6.0] - 2026-05-18

### Added

- `LogFmt` for explicit level-based formatted logging.
- Regression coverage for filtered and silent formatted log calls.
- Real multi-threaded logging coverage in the FPCUnit suite.

### Changed

- Simplified thread safety by using one unit-level lock instead of storing a critical section inside each `TSimpleLog` record.
  - Record copies and method chaining no longer copy critical-section internals.
  - `Finalize` is retained as a no-op compatibility method; no per-instance cleanup is required.
- Formatted logging overloads now skip `Format` when the message would be filtered by `MinLevel` or suppressed by `Silent`.
- File rotation now uses millisecond timestamped backup names and avoids overwriting an existing backup from the same timestamp.
- Tests run faster by removing artificial sleeps and debug output.

### Fixed

- Fixed stale documentation that still referenced `Flush` and `.1` rotation backups.
- Fixed the standalone manual test program to use `TSimpleLog`.
- Improved test cleanup so nested temporary log directories are removed.

## Release [0.5.1] - 2025-07-04

### Added

- **Thread Safety**: All logging operations are now protected by a critical section for true thread safety
  - `TSimpleLog` now uses a `TRTLCriticalSection` internally
  - Safe to use the same logger instance from multiple threads
  - New `Finalize` method to release resources (optional, for advanced use)

### Changed

- Improved documentation for thread safety and resource management
- Updated user manual and cheat sheet to reflect thread-safe design

### Fixed

- Minor documentation and test clarifications

## Release [0.5.0] - 2025-07-02

### Added

- **New TSimpleLog Implementation**: Complete rewrite using Free Pascal advanced records
  - Value semantics with automatic memory management (no Create/Free required)
  - Method chaining for fluent configuration
  - Factory methods: `Console`, `FileLog`, `Both`
  - Zero dependencies - uses only standard Free Pascal units (Classes, SysUtils)

- **Enhanced Cross-Platform Support**:
  - Windows console colors via Windows API
  - Unix/Linux console colors via ANSI escape codes
  - Consistent behavior across all supported platforms

- **Improved File Rotation**:
  - Automatic rotation when files exceed size limit
  - Configurable maximum file size with minimum threshold
  - Simple rotation strategy (file.log → file.log.1)
  - Built-in protection against I/O overload

- **Modern API Features**:
  - Method chaining: `TSimpleLog.Both('app.log').SetMinLevel(llInfo).SetMaxFileSize(5*1024*1024)`
  - Format string support for all log methods
  - Silent mode for testing and quiet operations
  - Property access to all configuration options

- **Comprehensive Documentation**:
  - New SimpleLogger.md user manual with complete API reference
  - Updated cheat sheet with practical examples
  - Migration guide from old Logger-FP
  - Best practices and common usage patterns

- **Robust Testing Suite**:
  - 25 comprehensive test cases covering all functionality
  - File rotation edge cases and validation
  - Multi-output logging verification
  - Method chaining and configuration testing


### Fixed

- **File Rotation Issues**:
  - Fixed rotation logic to rename files correctly
  - Added minimum file size threshold to prevent excessive I/O
  - Improved error handling for file operations
  - Ensured new files are created after rotation

- **Error Handling**:
  - Silent failure for logging errors (doesn't crash applications)
  - Graceful handling of invalid file paths
  - Automatic directory creation for log files
  - Robust cross-platform path handling

### Removed

- Removed Flush method (was a no-op, all writes are immediate)
