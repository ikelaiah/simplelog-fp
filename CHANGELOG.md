# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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