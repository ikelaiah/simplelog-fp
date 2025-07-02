# 📋 SimpleLog Cheat Sheet

A quick reference for SimpleLog usage and features.

## Table of Contents

- [📋 SimpleLog Cheat Sheet](#-simplelog-cheat-sheet)
  - [Table of Contents](#table-of-contents)
  - [📝 Quick Start](#-quick-start)
  - [🏭 Factory Methods](#-factory-methods)
  - [⚙️ Configuration](#️-configuration)
  - [📊 Log Levels](#-log-levels)
  - [🔄 Method Chaining](#-method-chaining)
  - [📁 File Operations](#-file-operations)
  - [🎯 Common Patterns](#-common-patterns)
  - [🚀 Best Practices](#-best-practices)

## 📝 Quick Start

```pascal
uses SimpleLog;

var Logger: TSimpleLog;

// 1. Create logger
Logger := TSimpleLog.Console;                    // Console only
Logger := TSimpleLog.FileLog('app.log');         // File only  
Logger := TSimpleLog.Both('app.log');            // Both console and file

// 2. Configure (optional)
Logger := Logger.SetMinLevel(llInfo).SetMaxFileSize(5 * 1024 * 1024);

// 3. Log messages
Logger.Info('Application started');
Logger.Warning('Low memory: %d MB remaining', [memoryMB]);
Logger.Error('Failed to save file: %s', [fileName]);

// 4. Cleanup (optional for file logging)
Logger.Flush;
```

## 🏭 Factory Methods

```pascal
// Console logging only
Logger := TSimpleLog.Console;

// File logging only
Logger := TSimpleLog.FileLog('application.log');

// Both console and file
Logger := TSimpleLog.Both('application.log');
```

## ⚙️ Configuration

```pascal
// Set minimum log level
Logger := Logger.SetMinLevel(llInfo);           // Only Info and above
Logger := Logger.SetMinLevel(llWarning);        // Only Warning, Error, Fatal

// Set maximum file size (automatic rotation)
Logger := Logger.SetMaxFileSize(10 * 1024 * 1024);  // 10MB

// Change output destinations
Logger := Logger.SetOutputs([odConsole]);       // Console only
Logger := Logger.SetOutputs([odFile]);          // File only
Logger := Logger.SetOutputs([odConsole, odFile]); // Both

// Change log file
Logger := Logger.SetFile('new-log.log');

// Silent mode (suppress all output)
Logger := Logger.SetSilent(True);
Logger := Logger.SetSilent(False);              // Re-enable output
```

## 📊 Log Levels

```pascal
// Available log levels (lowest to highest)
Logger.Debug('Detailed debug information');     // llDebug
Logger.Info('General information');             // llInfo  
Logger.Warning('Warning message');              // llWarning
Logger.Error('Error occurred');                 // llError
Logger.Fatal('Critical failure');               // llFatal

// Format string versions
Logger.Debug('Variable value: %d', [value]);
Logger.Info('User %s logged in', [username]);
Logger.Warning('Memory usage: %d%%', [percentage]);
Logger.Error('File not found: %s', [fileName]);
Logger.Fatal('Database connection failed: %s', [errorMsg]);

// Generic log method
Logger.Log(llInfo, 'Custom log level message');
```

## 🔄 Method Chaining

```pascal
// Chain multiple configuration calls
Logger := TSimpleLog.Both('application.log')
  .SetMinLevel(llInfo)
  .SetMaxFileSize(5 * 1024 * 1024)
  .SetSilent(False);

// Reconfigure existing logger
Logger := Logger
  .SetOutputs([odFile])
  .SetFile('debug.log')
  .SetMinLevel(llDebug);
```

## 📁 File Operations

```pascal
// File rotation happens automatically when MaxFileSize is reached
// application.log → application.log.1 (backup)
// New application.log is created

// Ensure data is written to file
Logger.Flush;

// Check if logging to file
if odFile in Logger.Outputs then
  Logger.Info('Logging to file: %s', [Logger.LogFile]);

// Access properties
WriteLn('Current log file: ', Logger.LogFile);
WriteLn('Max file size: ', Logger.MaxFileSize);
WriteLn('Min level: ', Ord(Logger.MinLevel));
WriteLn('Silent mode: ', Logger.Silent);
```

## 🎯 Common Patterns

```pascal
// Development logging
DevLogger := TSimpleLog.Console.SetMinLevel(llDebug);
DevLogger.Debug('Development info here');

// Production logging  
ProdLogger := TSimpleLog.FileLog('production.log').SetMinLevel(llWarning);
ProdLogger.Error('Production error logged');

// Both console and file with rotation
AppLogger := TSimpleLog.Both('app.log')
  .SetMinLevel(llInfo)
  .SetMaxFileSize(10 * 1024 * 1024);  // 10MB rotation

// Silent logger for testing
TestLogger := TSimpleLog.Console.SetSilent(True);
TestLogger.Info('This will not be displayed');

// Multiple loggers for different components
UILogger := TSimpleLog.FileLog('ui.log').SetMinLevel(llInfo);
DBLogger := TSimpleLog.FileLog('database.log').SetMinLevel(llWarning);
NetworkLogger := TSimpleLog.Both('network.log').SetMinLevel(llDebug);
```

## 🚀 Best Practices

```pascal
// ✅ DO: Use method chaining for clean configuration
Logger := TSimpleLog.Both('app.log').SetMinLevel(llInfo).SetMaxFileSize(5_000_000);

// ✅ DO: Use format strings instead of string concatenation
Logger.Info('Processing %d items', [count]);

// ❌ AVOID: String concatenation in logging calls
Logger.Info('Processing ' + IntToStr(count) + ' items');  // Slower

// ✅ DO: Choose appropriate log levels
Logger.Debug('Technical details for debugging');          // Development only
Logger.Info('Normal operational events');                 // General info
Logger.Warning('Something unusual but not critical');     // Potential issues
Logger.Error('Recoverable error occurred');               // Handle and continue
Logger.Fatal('Critical error, cannot continue');          // Application must stop

// ✅ DO: Configure once, use everywhere
function CreateAppLogger: TSimpleLog;
begin
  Result := TSimpleLog.Both('application.log')
    .SetMinLevel(llInfo)
    .SetMaxFileSize(10 * 1024 * 1024);
end;

// ✅ DO: Call Flush for critical logging
Logger.Fatal('Critical system error detected');
Logger.Flush;  // Ensure message is written immediately

// ✅ DO: Use different loggers for different purposes
var
  AppLogger: TSimpleLog;      // General application events
  ErrorLogger: TSimpleLog;    // Error-only logging
  DebugLogger: TSimpleLog;    // Development debugging
begin
  AppLogger := TSimpleLog.FileLog('app.log').SetMinLevel(llInfo);
  ErrorLogger := TSimpleLog.FileLog('errors.log').SetMinLevel(llError);
  DebugLogger := TSimpleLog.Console.SetMinLevel(llDebug);
end;

// ✅ DO: Handle file path issues gracefully
// SimpleLog will handle invalid paths silently - your app won't crash
Logger := TSimpleLog.FileLog('/invalid/path/app.log');
Logger.Info('This handles errors gracefully');
```

## Migration from Old Logger-FP

```pascal
// OLD Logger-FP style
uses Logger;
TLogger.CreateConsoleAndFileLogger('app.log', llInfo);
Logger.Info('Message');

// NEW SimpleLog style  
uses SimpleLog;
var Logger: TSimpleLog;
Logger := TSimpleLog.Both('app.log').SetMinLevel(llInfo);
Logger.Info('Message');
```


