# SimpleLog-FP Manual

## Overview

SimpleLog-FP is a focused logging unit for Free Pascal applications. It writes
timestamped messages to the console, a file, or both without requiring object
lifetime management or third-party dependencies.

The library is intended for single-process applications that need straightforward
text logs. It does not provide structured events, category hierarchies, network
sinks, or cross-process file coordination.

## Quick Start

```pascal
uses
  SimpleLog;

var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Both('application.log')
    .SetMinLevel(llInfo);

  Log.Info('Application started');
  Log.Warning('Low disk space: %d%% remaining', [8]);
end;
```

Always initialize a logger with `Console`, `FileLog`, or `Both`. A default,
zero-initialized `TSimpleLog` has no output destinations.

## Factories

```pascal
class function Console: TSimpleLog; static;
class function FileLog(const AFileName: string): TSimpleLog; static;
class function Both(const AFileName: string): TSimpleLog; static;
```

Factory defaults are:

- Minimum level: `llDebug`
- Maximum file size: 10 MiB
- Silent mode: disabled
- Console color mode: `ccmAuto`
- File error mode: `lemSilent`

## Logging

```pascal
Log.Debug('Diagnostic detail');
Log.Info('Normal operation');
Log.Warning('Potential problem');
Log.Error('Recoverable failure');
Log.Fatal('Critical failure');

Log.Info('Processed %d records', [RecordCount]);
Log.Log(llInfo, 'A message with an explicit level');
```

`Fatal` records the `llFatal` severity; it does not terminate the application.
The caller remains responsible for shutdown or recovery.

Formatted overloads do not call `Format` when their level is filtered or silent
mode is active.

## Configuration

Configuration methods update the record and return it for chaining:

```pascal
Log := TSimpleLog.Both('application.log')
  .SetOutputs([odConsole, odFile])
  .SetFile('application.log')
  .SetMinLevel(llInfo)
  .SetMaxFileSize(5 * 1024 * 1024)
  .SetSilent(False)
  .SetColorMode(ccmAuto)
  .SetErrorMode(lemSilent);
```

Available methods:

```pascal
function SetOutputs(AOutputs: TOutputDestinations): TSimpleLog;
function SetFile(const AFileName: string): TSimpleLog;
function SetMinLevel(ALevel: TLogLevel): TSimpleLog;
function SetMaxFileSize(ASize: Int64): TSimpleLog;
function SetSilent(ASilent: Boolean): TSimpleLog;
function SetColorMode(AMode: TConsoleColorMode): TSimpleLog;
function SetUseColors(AUseColors: Boolean): TSimpleLog;
function SetErrorMode(AMode: TLogErrorMode): TSimpleLog;
```

`SetMaxFileSize` clamps values below 1 KiB to 1 KiB.

`SetUseColors` is a convenience API retained for compatibility:

- `SetUseColors(True)` selects `ccmAlways`
- `SetUseColors(False)` selects `ccmNever`

## Console Colors

```pascal
TConsoleColorMode = (ccmAuto, ccmAlways, ccmNever);
```

- `ccmAuto` is the default. Colors are used only when the current `Output` text
  handle is an interactive terminal.
- `ccmAlways` requests colors even when output is redirected. On Unix this
  writes ANSI escape sequences.
- `ccmNever` writes plain console text.

On Windows, SimpleLog restores the console attributes that were active before
each message rather than imposing a hardcoded default.

All console levels currently write to standard `Output`. SimpleLog does not
route warnings or errors to `StdErr` automatically.

## File Error Policy

```pascal
TLogErrorMode = (lemSilent, lemRaise);
```

`lemSilent` is the default and preserves the traditional best-effort behavior:
directory, file, and rotation failures do not escape the logging call.

Use `lemRaise` when the application must observe failures:

```pascal
Log := TSimpleLog.FileLog('required/application.log')
  .SetErrorMode(lemRaise);

try
  Log.Info('Required audit event');
except
  on E: ESimpleLogError do
    WriteLn(StdErr, E.Message);
end;
```

With `Both`, console output is written before file output. A file exception can
therefore occur after the message has already appeared on the console.

## File Rotation

When the active file is already at or above `MaxFileSize` before a write:

1. The previous `.1` backup is staged temporarily when necessary.
2. The active file is renamed to `.1`.
3. A new active file is created by the pending write.
4. The previous staged backup is removed.

If rotation cannot safely rename the active file, SimpleLog leaves the active
file intact. In `lemSilent` mode it continues appending to that file; in
`lemRaise` mode it raises `ESimpleLogError`.

The size check occurs before writing. A single entry can therefore make the
active file larger than `MaxFileSize`, and an individual oversized entry can
produce an oversized backup on the next write.

Rotation is designed for one process. Multiple processes must not write or
rotate the same file concurrently.

## Timestamps

The fixed text format is:

```text
[yyyy-mm-dd hh:nn:ss.zzz] [LEVEL] message
```

The timestamp comes from `Now` and represents local wall-clock time. It displays
milliseconds but does not include a UTC offset or timezone. Wall-clock time can
jump when the system clock changes and should not be used as a monotonic
performance timer.

Messages containing line endings produce multi-line log entries; SimpleLog does
not escape message content.

## Thread-Safety Contract

SimpleLog uses one unit-level critical section:

- Logging calls are serialized within the process.
- Each configuration method is serialized.
- Read-only property access is serialized.
- Formatted logging makes the filter decision and formats the message under the
  same lock.
- Independent logger values are also serialized because the lock is global.

A multi-method configuration chain is not one atomic operation. Configure a
logger before sharing it when other threads must not observe intermediate
settings.

`TSimpleLog` has value semantics. Passing or assigning it by value copies the
configuration; later changes to one copy do not update another copy.

On Unix, the host program must initialize thread support in the normal Free
Pascal way by placing `cthreads` first in its program `uses` list before using
threads.

## Read-Only Properties

```pascal
property Outputs: TOutputDestinations;
property LogFile: string;
property MinLevel: TLogLevel;
property MaxFileSize: Int64;
property Silent: Boolean;
property ColorMode: TConsoleColorMode;
property UseColors: Boolean;
property ErrorMode: TLogErrorMode;
```

`UseColors` is `False` only for `ccmNever`; inspect `ColorMode` when the
distinction between automatic and always-on behavior matters.

## Enumerations

```pascal
TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

TOutputDestination = (odConsole, odFile);
TOutputDestinations = set of TOutputDestination;

TConsoleColorMode = (ccmAuto, ccmAlways, ccmNever);
TLogErrorMode = (lemSilent, lemRaise);
```

## Lazarus Package

Open `packages/lazarus/simplelog_fp.lpk` and add the `simplelog_fp` package to
the project. The package name intentionally differs from the public `SimpleLog`
unit name so the generated package unit cannot shadow the library.

## Resource Management

`TSimpleLog` is an advanced record. It does not require `Create`, `Free`,
`Finalize`, or `Flush`; each file write is completed before the logging call
returns.
