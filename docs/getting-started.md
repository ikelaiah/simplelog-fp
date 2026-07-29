# Getting Started

This guide shows the common SimpleLog-FP path in a few small steps.

## 1. Add the Unit

```pascal
uses
  SimpleLog;
```

## 2. Create a Logger

```pascal
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Console;
  Log.Info('Hello from SimpleLog-FP');
end;
```

## 3. Log to a File

```pascal
Log := TSimpleLog.FileLog('app.log');
Log.Info('Application started');
```

SimpleLog creates missing directories for file targets when it can.

## 4. Log to Console and File

```pascal
Log := TSimpleLog.Both('app.log')
  .SetMinLevel(llInfo);

Log.Debug('This is hidden');
Log.Info('This is shown');
```

## 5. Use Format Strings

```pascal
Log.Info('Processed %d records', [42]);
Log.Error('Could not open %s', ['settings.ini']);
```

## 6. Rotate Files

```pascal
Log := TSimpleLog.FileLog('app.log')
  .SetMaxFileSize(5 * 1024 * 1024);
```

When `app.log` reaches the limit, it is moved to `app.log.1`. SimpleLog keeps only that one backup.

## 7. Control Console Colors

```pascal
Log := TSimpleLog.Console.SetColorMode(ccmNever);
```

The default `ccmAuto` mode uses colors only when `Output` is an interactive
terminal. Use `ccmAlways` or `ccmNever` to override automatic detection.

## 8. Choose a File Error Policy

File and rotation failures are non-fatal by default:

```pascal
Log := TSimpleLog.FileLog('app.log').SetErrorMode(lemSilent);
```

Applications that must observe logging failures can opt into exceptions:

```pascal
Log := TSimpleLog.FileLog('app.log').SetErrorMode(lemRaise);

try
  Log.Info('Application started');
except
  on E: ESimpleLogError do
    WriteLn(StdErr, E.Message);
end;
```

## Notes

- Logging calls, individual configuration calls, and property reads are
  serialized within the process.
- Configure before sharing when a multi-step configuration chain must be
  observed as one logical change.
- Use the `Set...` methods for configuration.
- Properties such as `LogFile`, `MinLevel`, `ColorMode`, and `ErrorMode` are
  read-only status values.
- Timestamps use local wall-clock time and do not include a timezone offset.
