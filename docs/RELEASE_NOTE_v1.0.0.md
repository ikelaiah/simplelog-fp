# 🚀 SimpleLog-FP v1.0.0

> **The first stable release is here!**

SimpleLog-FP v1.0.0 is the first stable release of the focused logging library
for Free Pascal.

It keeps the API small—console logging, file logging, or both—while strengthening
the behavior expected from a dependable application logger.

## ✨ Highlights

### 🛡️ Safer file rotation

Failed rotation no longer deletes or truncates the active log as a fallback.
SimpleLog-FP stages the previous backup and preserves log data when filesystem
operations fail.

Rotation continues to keep one normal backup:

```text
application.log
application.log.1
```

### 🚨 Observable file errors

Logging remains best-effort by default:

```pascal
Log := TSimpleLog.FileLog('application.log')
  .SetErrorMode(lemSilent);
```

Applications that require confirmation of file output can opt into exceptions:

```pascal
Log := TSimpleLog.FileLog('application.log')
  .SetErrorMode(lemRaise);

try
  Log.Info('Required log entry');
except
  on E: ESimpleLogError do
    WriteLn(StdErr, E.Message);
end;
```

### 🎨 Smarter console colors

Console colors now default to automatic terminal detection:

```pascal
Log := TSimpleLog.Console.SetColorMode(ccmAuto);
```

Available modes are:

- `ccmAuto` — color only when `Output` is an interactive terminal
- `ccmAlways` — always request colored output
- `ccmNever` — always write plain output

On Windows, the console attributes active before a message are restored
afterward. Redirected Unix output no longer receives ANSI sequences in automatic
mode.

### 🧵 Clearer thread-safety guarantees

Logging calls, individual configuration methods, formatted filtering, and
read-only property access are serialized within the process.

`TSimpleLog` retains value semantics: copying a logger copies its configuration,
and later changes to one copy do not update another.

### 📦 Tested Lazarus package

The Lazarus package is now named `simplelog_fp`. This avoids a unit-name
collision that previously allowed the generated package unit to shadow the
public `SimpleLog` unit.

The release includes a package-only consumer test to verify that applications
can compile against the package output.

## ⬆️ Upgrade Guide

### 🧰 Lazarus package users

Remove the old `simplelog` package reference and add:

```text
packages/lazarus/simplelog_fp.lpk
```

Your Pascal source still imports the same public unit:

```pascal
uses
  SimpleLog;
```

### 🌈 Console color behavior

Colors now use `ccmAuto` by default. To retain unconditional color behavior:

```pascal
Log := Log.SetColorMode(ccmAlways);
```

To force plain output:

```pascal
Log := Log.SetColorMode(ccmNever);
```

`SetUseColors(True)` and `SetUseColors(False)` remain available as convenience
aliases.

## 🚀 Quick Example

```pascal
program MyApplication;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleLog;

var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Both('logs/application.log')
    .SetMinLevel(llInfo)
    .SetMaxFileSize(5 * 1024 * 1024)
    .SetColorMode(ccmAuto)
    .SetErrorMode(lemSilent);

  Log.Info('Application started');
  Log.Warning('Example warning: %d', [42]);
end.
```

## 📝 Log Format

```text
[yyyy-mm-dd hh:nn:ss.zzz] [LEVEL] message
```

Timestamps use local wall-clock time, display milliseconds, and do not include a
timezone or UTC offset. They are intended for readable event ordering, not
monotonic performance measurement.

`Fatal` records the `llFatal` severity but does not terminate the application.

## ✅ Quality and Compatibility

- Free Pascal 3.2.2 or later
- Lazarus 4.0 or later
- Windows and Unix-family platforms
- 39 automated FPCUnit tests
- Debug runtime checks and heap tracing
- Windows Lazarus and Ubuntu FPC CI coverage
- No third-party runtime dependencies

## 🎯 Known Scope

SimpleLog-FP is designed for single-process text logging. It intentionally does
not provide:

- Cross-process file coordination
- Structured or JSON logging
- Network logging sinks
- Logger category hierarchies
- Automatic application termination for fatal messages

## 📥 Installation

Copy the public unit into a project:

```text
src/SimpleLog.pas
```

or add the Lazarus package:

```text
packages/lazarus/simplelog_fp.lpk
```

Then:

```pascal
uses
  SimpleLog;
```

## 🙏 Thank You

Thank you to everyone who tested, reviewed, suggested improvements, and helped
SimpleLog-FP reach its first stable release. 💙
