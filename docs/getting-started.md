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

## 7. Disable Colors When Needed

```pascal
Log := TSimpleLog.Console.SetUseColors(False);
```

This is useful for CI logs, redirected output, and terminals that do not handle color control codes well.

## Notes

- Configure the logger before sharing it between threads.
- Use the `Set...` methods for configuration.
- Properties such as `LogFile`, `MinLevel`, and `UseColors` are read-only status values.
