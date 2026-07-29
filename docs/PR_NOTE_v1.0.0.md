# Prepare SimpleLog-FP v1.0.0

## Summary

This pull request prepares SimpleLog-FP for its first stable release.

Version 1.0.0 keeps the library's focused, single-unit design while improving
package integration, log-file safety, error visibility, console behavior,
thread-safety guarantees, tests, and documentation.

## Highlights

- Renamed the Lazarus package to `simplelog_fp` so its generated package unit
  cannot shadow the public `SimpleLog` unit.
- Made file rotation non-destructive when a rename, backup, or cleanup step
  fails.
- Added an opt-in exception policy for file and rotation errors.
- Added automatic terminal color detection with explicit always/never modes.
- Synchronized logging decisions, configuration calls, formatted messages, and
  property reads.
- Expanded the test suite to 39 tests.
- Added a package-only consumer test to validate the compiled Lazarus package.
- Updated CI, examples, documentation, version metadata, and release guidance.
- Redesigned the project banner for the v1.0.0 release.

## Public API Changes

### Console color policy

```pascal
TConsoleColorMode = (ccmAuto, ccmAlways, ccmNever);
```

```pascal
Log := TSimpleLog.Console.SetColorMode(ccmAuto);
```

`ccmAuto` is the new default. It uses colors only when the current `Output`
handle is an interactive terminal.

The existing `SetUseColors` method remains available:

- `SetUseColors(True)` selects `ccmAlways`
- `SetUseColors(False)` selects `ccmNever`

### File error policy

```pascal
TLogErrorMode = (lemSilent, lemRaise);
```

```pascal
Log := TSimpleLog.FileLog('application.log')
  .SetErrorMode(lemRaise);
```

`lemSilent` remains the default for best-effort logging. `lemRaise` reports file
and rotation failures through `ESimpleLogError`.

## Rotation Safety

Rotation now stages the existing backup before replacing it. If the active log
cannot be safely renamed:

- The active log is not deleted or truncated.
- The previous backup is restored when possible.
- `lemSilent` continues appending to the active file.
- `lemRaise` reports the failure as `ESimpleLogError`.

## Thread-Safety Contract

SimpleLog-FP uses one unit-level critical section:

- Logging calls are serialized within the process.
- Configuration methods are serialized.
- Property reads are serialized.
- Formatted logging performs filtering and formatting under the same lock.

Multi-method configuration chains are not atomic. Configure a logger before
sharing it when other threads must not observe intermediate settings.

## Breaking Changes

- The Lazarus package was renamed from `simplelog` to `simplelog_fp`.
- Console colors now default to `ccmAuto` instead of being unconditionally
  enabled.

The public Pascal unit remains `SimpleLog`, so source installations continue to
use:

```pascal
uses
  SimpleLog;
```

## Validation

- [x] Debug build succeeds with runtime checks and heap tracing.
- [x] Release build succeeds.
- [x] All 39 FPCUnit tests pass.
- [x] Debug test run reports zero memory leaks.
- [x] Lazarus package builds the real `SimpleLog` unit.
- [x] Package-only consumer compiles and runs.
- [x] Basic and thread-safe examples compile in Release mode.
- [x] Plain FPC checked build succeeds.
- [x] Documentation matches the v1.0.0 implementation.
- [x] `git diff --check` passes.

## Merge Checklist

- [ ] Review the API and breaking-change notes.
- [ ] Confirm Windows and Ubuntu CI checks pass.
- [ ] Merge into `main`.
- [ ] Verify `main` after merge.
- [ ] Create the signed `v1.0.0` tag.
- [ ] Publish the GitHub release using
      [`RELEASE_NOTE_v1.0.0.md`](RELEASE_NOTE_v1.0.0.md).
