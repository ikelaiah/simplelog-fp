# SimpleLog Documentation

## Overview

`SimpleLog` is a minimal, modern logging component for Free Pascal applications. It provides formatted log messages with timestamps, log levels, automatic file rotation, cross-platform colored console output, and multiple output destinations.

## Philosophy

SimpleLog follows a "minimal but complete" philosophy:

- **Zero dependencies** - Uses only standard Free Pascal units
- **Cross-platform** - Works on Windows, Linux, macOS without changes
- **Developer-friendly** - Method chaining, fluent API, sensible defaults
- **Production-ready** - File rotation, thread safety, error handling

## Target Audience

SimpleLog is designed for:

- **Free Pascal developers** seeking a modern, dependency-free logging solution
- **Developers who value simplicity** but need reliable logging capabilities
- **Applications ranging from small utilities to complex systems**

You can expect:

- One-line setup for common scenarios
- Method chaining for fluent configuration
- Automatic file rotation and directory creation
- Cross-platform colored console output

You won't find:

- Complex hierarchical logger trees
- Heavy dependencies or framework requirements
- Performance overhead from unused features

## Quick Start Guide

This step-by-step guide will help you get started with SimpleLog:

### Step 1: Include the SimpleLog Unit

```pascal
uses
  SimpleLog;
```

### Step 2: Create a Logger

SimpleLog uses factory methods to create logger instances:

```pascal
var
  Logger: TSimpleLog;
begin
  // Console only
  Logger := TSimpleLog.Console;
  
  // File only
  Logger := TSimpleLog.FileLog('application.log');
  
  // Both console and file
  Logger := TSimpleLog.Both('application.log');
end;
```

### Step 3: Configure with Method Chaining (Optional)

```pascal
var
  Logger: TSimpleLog;
begin
  Logger := TSimpleLog.Both('application.log')
    .SetMinLevel(llInfo)
    .SetMaxFileSize(5 * 1024 * 1024);  // 5MB
end;
```

### Step 4: Log Messages

Choose the appropriate log level based on the message importance:

```pascal
// Development-time detailed information
Logger.Debug('Database connection string: %s', [connectionString]);

// General operational information
Logger.Info('User %s logged in successfully', [username]);

// Potential issue that might need attention
Logger.Warning('Disk space below 10%% on drive %s', [driveLetter]);

// Error that allows operation to continue
Logger.Error('Could not save preferences: %s', [errorMessage]);

// Critical error that requires application termination
Logger.Fatal('Database connection lost, cannot continue');
```

## Common Scenarios

Here are some common logging scenarios and how to handle them:

### Development and Debugging

```pascal
var
  Logger: TSimpleLog;
begin
  // Show all details including debug messages
  Logger := TSimpleLog.Console.SetMinLevel(llDebug);

  // Include technical details useful for debugging
  Logger.Debug('Connection status: %d, buffer size: %d', [connStatus, bufferSize]);
end;
```

### Production Environment

```pascal
var
  Logger: TSimpleLog;
begin
  // Log to file with reduced noise
  Logger := TSimpleLog.FileLog('application.log').SetMinLevel(llInfo);

  // Or even more restrictive
  Logger := TSimpleLog.FileLog('application.log').SetMinLevel(llWarning);
end;
```

### Both Console and File Logging

```pascal
var
  Logger: TSimpleLog;
begin
  // Log to both console and file
  Logger := TSimpleLog.Both('application.log').SetMinLevel(llInfo);
  
  Logger.Info('Application started');
  Logger.Warning('Low memory detected');
end;
```

### File Rotation

```pascal
var
  Logger: TSimpleLog;
begin
  // Automatically rotate files when they reach 5MB
  Logger := TSimpleLog.FileLog('application.log')
    .SetMaxFileSize(5 * 1024 * 1024);  // 5MB
    
  // Files will be rotated as: application.log -> application.log.1
  Logger.Info('This will be logged with automatic rotation');
end;
```

### Silent Logging

```pascal
var
  Logger: TSimpleLog;
begin
  // Suppress all output (useful for testing or quiet modes)
  Logger := TSimpleLog.Console.SetSilent(True);
  
  Logger.Info('This message will not be displayed');
end;
```

## Features

- **Log Levels**: Debug, Info, Warning, Error, and Fatal levels with cross-platform console colors
- **Multiple Destinations**: Log to console, files, or both simultaneously  
- **File Rotation**: Automatic log file rotation when size limits are reached
- **Method Chaining**: Fluent API for configuration (SetMinLevel, SetMaxFileSize, etc.)
- **Zero Dependencies**: Uses only standard Free Pascal units (Classes, SysUtils)
- **Cross-Platform**: Works on Windows, Linux, macOS without changes
- **Thread-Safe**: Basic thread safety through controlled file operations
- **Error Handling**: Robust error recovery to prevent logging failures from crashing applications
- **Format String Support**: Convenient format string overloads for all log methods
- **Automatic Directory Creation**: Creates log directories automatically
- **Timestamped Output**: All log messages include precise timestamps
- **Silent Mode**: Ability to suppress all output for testing or quiet operations
- **Advanced Record**: TSimpleLog is an advanced record with value semantics

## Advanced Usage

### Complete Configuration Example

```pascal
var
  Logger: TSimpleLog;
begin
  Logger := TSimpleLog.Both('application.log')
    .SetMinLevel(llInfo)
    .SetMaxFileSize(10 * 1024 * 1024)  // 10MB
    .SetSilent(False);
    
  Logger.Info('Logger configured with method chaining');
end;
```

### Dynamic Output Configuration

```pascal
var
  Logger: TSimpleLog;
begin
  Logger := TSimpleLog.Console;
  
  // Change to file logging at runtime
  Logger := Logger
    .SetOutputs([odFile])
    .SetFile('new-logfile.log');
    
  // Change to both outputs
  Logger := Logger.SetOutputs([odConsole, odFile]);
end;
```

### Error Handling

```pascal
var
  Logger: TSimpleLog;
begin
  // SimpleLog handles errors gracefully - failed writes won't crash your app
  Logger := TSimpleLog.FileLog('/invalid/path/app.log');
  Logger.Info('This will fail silently if path is invalid');
  
  // Always check if logging is working in critical scenarios
  if FileExists(Logger.LogFile) then
    Logger.Info('Logging is working correctly');
end;
```

### Working with Multiple Loggers

```pascal
var
  AppLogger, DebugLogger: TSimpleLog;
begin
  // Different loggers for different purposes
  AppLogger := TSimpleLog.FileLog('application.log').SetMinLevel(llInfo);
  DebugLogger := TSimpleLog.Console.SetMinLevel(llDebug);
  
  AppLogger.Info('Application event');
  DebugLogger.Debug('Debug information');
end;
```

## API Reference

### TSimpleLog Record

TSimpleLog is an advanced record (value type) that provides logging functionality with method chaining.

#### Factory Methods

```pascal
class function Console: TSimpleLog; static;
class function FileLog(const AFileName: string): TSimpleLog; static;
class function Both(const AFileName: string): TSimpleLog; static;
```

**Examples:**

```pascal
var
  Logger: TSimpleLog;
begin
  // Console only
  Logger := TSimpleLog.Console;
  
  // File only
  Logger := TSimpleLog.FileLog('application.log');
  
  // Both console and file
  Logger := TSimpleLog.Both('application.log');
end;
```

#### Configuration Methods (Method Chaining)

```pascal
function SetOutputs(AOutputs: TOutputDestinations): TSimpleLog;
function SetFile(const AFileName: string): TSimpleLog;
function SetMinLevel(ALevel: TLogLevel): TSimpleLog;
function SetMaxFileSize(ASize: Int64): TSimpleLog;
function SetSilent(ASilent: Boolean): TSimpleLog;
```

**Examples:**

```pascal
var
  Logger: TSimpleLog;
begin
  Logger := TSimpleLog.Both('application.log')
    .SetMinLevel(llInfo)
    .SetMaxFileSize(5 * 1024 * 1024)  // 5MB
    .SetSilent(False);
end;
```

#### Logging Methods

```pascal
procedure Log(ALevel: TLogLevel; const AMessage: string);
procedure Debug(const AMessage: string);
procedure Info(const AMessage: string);
procedure Warning(const AMessage: string);
procedure Error(const AMessage: string);
procedure Fatal(const AMessage: string);
```

**Format String Overloads:**

```pascal
procedure Debug(const AFormat: string; const AArgs: array of const); overload;
procedure Info(const AFormat: string; const AArgs: array of const); overload;
procedure Warning(const AFormat: string; const AArgs: array of const); overload;
procedure Error(const AFormat: string; const AArgs: array of const); overload;
procedure Fatal(const AFormat: string; const AArgs: array of const); overload;
```

**Examples:**

```pascal
var
  Logger: TSimpleLog;
begin
  Logger := TSimpleLog.Console;
  
  // Simple messages
  Logger.Info('Application started');
  Logger.Warning('Low memory detected');
  
  // Format strings
  Logger.Info('User %s logged in at %s', [username, TimeToStr(Now)]);
  Logger.Error('Failed to save file: %s', [errorMessage]);
end;
```

#### Utility Methods

```pascal
procedure Flush;
```

Ensures all log data is written to files.

#### Properties

```pascal
property Outputs: TOutputDestinations read FOutputs write FOutputs;
property LogFile: string read FLogFile write FLogFile;
property MinLevel: TLogLevel read FMinLevel write FMinLevel;
property MaxFileSize: Int64 read FMaxFileSize write FMaxFileSize;
property Silent: Boolean read FSilent write FSilent;
```

### Enumerations

#### TLogLevel

```pascal
TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
```

- **llDebug**: Detailed information for debugging
- **llInfo**: General informational messages
- **llWarning**: Warning messages for potential issues
- **llError**: Error messages for failures that allow continued operation
- **llFatal**: Critical errors requiring application termination

#### TOutputDestination

```pascal
TOutputDestination = (odConsole, odFile);
TOutputDestinations = set of TOutputDestination;
```

- **odConsole**: Output to console/terminal
- **odFile**: Output to log file

### Constants

```pascal
DEFAULT_MAX_FILE_SIZE = 10 * 1024 * 1024; // 10MB
MIN_MAX_FILE_SIZE = 1024; // 1KB minimum
```

## File Rotation

SimpleLog automatically rotates log files when they reach the maximum size:

1. When a file reaches `MaxFileSize`, it's renamed with a `.1` extension
2. A new file is created with the original name
3. Only one backup file is kept (`.1`)

**Example:**

- `application.log` reaches 10MB
- Renamed to `application.log.1`
- New `application.log` is created

## Thread Safety

SimpleLog provides basic thread safety through:

- Controlled file operations (open/write/close for each message)
- No shared mutable state between threads
- Value semantics (each logger instance is independent)

## Error Handling

SimpleLog handles errors gracefully:

- Failed file operations don't crash the application
- Invalid paths are handled silently
- Console output continues even if file logging fails

## Best Practices

### 1. Use Method Chaining for Configuration

```pascal
var
  Logger: TSimpleLog;
begin
  Logger := TSimpleLog.Both('application.log')
    .SetMinLevel(llInfo)
    .SetMaxFileSize(5 * 1024 * 1024);
end;
```

### 2. Choose Appropriate Log Levels

```pascal
Logger.Debug('Detailed debug info');      // Development only
Logger.Info('Normal operations');         // General information
Logger.Warning('Potential issues');       // Things to watch
Logger.Error('Recoverable errors');       // Failures that allow continuation
Logger.Fatal('Critical failures');        // Application cannot continue
```

### 3. Use Format Strings for Performance

```pascal
// Good - format string
Logger.Info('Processing %d items', [count]);

// Avoid - string concatenation in performance-critical code
Logger.Info('Processing ' + IntToStr(count) + ' items');
```

### 4. Configure Once, Use Everywhere

```pascal
// Configure at application startup
var
  Logger: TSimpleLog;
begin
  Logger := TSimpleLog.Both('application.log').SetMinLevel(llInfo);
  
  // Pass logger to components or use as global variable
  ProcessData(Logger);
end;
```

### 5. Call Flush When Necessary

For file logging, you may want to ensure data is written immediately:

```pascal
Logger.Info('Critical operation completed');
Logger.Flush;  // Ensure data is written immediately
```

## Limitations

1. SimpleLog is designed for single-process applications
2. Only one backup file is kept during rotation (`.1` extension)
3. Basic thread safety through controlled file operations
4. Cross-platform support requires Free Pascal compilation

## Migration from Logger-FP

If migrating from the old Logger-FP implementation:

1. Change `uses Logger` to `uses SimpleLog`
2. Replace singleton calls like `Logger.Info()` with instance calls
3. Use factory methods instead of initialization procedures
4. Update method chaining syntax for configuration

**Old Style:**
```pascal
uses Logger;
TLogger.CreateConsoleAndFileLogger('app.log', llInfo);
Logger.Info('Message');
```

**New Style:**
```pascal
uses SimpleLog;
var Logger: TSimpleLog;
Logger := TSimpleLog.Both('app.log').SetMinLevel(llInfo);
Logger.Info('Message');
```