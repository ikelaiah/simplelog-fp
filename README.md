# 📝 SimpleLog-FP 

[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-60A5FA.svg)](https://www.lazarus-ide.org/)
![Supports Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Supports Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
[![Version](https://img.shields.io/badge/version-0.5.1-8B5CF6.svg)](CHANGELOG.md)
![No Dependencies](https://img.shields.io/badge/dependencies-none-10B981.svg)

> [!Note]
> SimpleLog-FP is currently in active development. This is a pre-1.0 release (v0.5.1). The API may change and feedback is welcome!


A **simple**, **lightweight**, and **easy-to-use** logging library for Free Pascal applications.

> **Why SimpleLog-FP?** 🤔 Built from the need for a logging library that is actually simple to use and maintain. No feature bloat, no complexity - just clean, reliable logging for console, file, or both.

## 🎯 Design Philosophy

- ✅ **Simple** - Easy for new developers to understand and use
- ✅ **Lightweight** - ~415 lines of code, no bloat
- ✅ **Maintainable** - Clean code and easily maintainable
- ✅ **Focused** - Does three things well: console, file, and console+file logging
- ✅ **No dependencies** - Uses only standard Free Pascal units
- ✅ **Cross-platform** - Works on Windows and Unix systems
- ✅ **File rotation** - Automatic log file rotation when size limits are reached

## 🚀 Quick Start

### 📦 Installation

**From GitHub:**

```bash
git clone https://github.com/ikelaiah/simplelog-fp.git
cd simplelog-fp
```

**Manual Installation:**

1. Download `src/SimpleLog.pas` from this repository
2. Copy it to your project directory
2. Add `SimpleLog` to your uses clause
3. Start logging!

### Basic Usage

```pascal
uses SimpleLog;

var
  Log: TSimpleLog;
begin
  // Console only
  Log := TSimpleLog.Console;
  Log.Info('Hello World!');
  
  // File only  
  Log := TSimpleLog.FileLog('app.log');
  Log.Info('Logged to file');
  
  // Both console and file
  Log := TSimpleLog.Both('app.log');
  Log.Info('Appears in both console and file');
end;
```

**That's it!** No complex setup, no memory management, no configuration files required.

## ✨ Features

### 📝 Core Logging

- **5 log levels**: Debug, Info, Warning, Error, Fatal
- **Timestamped messages**: Each log entry includes precise timestamp with milliseconds
- **Colored console output** with appropriate colors per level
- **File logging** with automatic directory creation
- **Dual output** to both console and file simultaneously
- **Format string support** for all log methods
- **Thread safety** - protected by critical sections for multi-threaded applications
- **Silent mode** to temporarily disable all logging output

### Log Output Format

All log messages follow a consistent, readable format:

```
[2025-07-01 14:30:22.123] [INFO] Application started
[2025-07-01 14:30:22.456] [WARNING] Low disk space: 15.5% remaining
[2025-07-01 14:30:22.789] [ERROR] Database connection failed
[2025-07-01 14:30:23.012] [DEBUG] User authentication successful
[2025-07-01 14:30:23.345] [FATAL] Critical system error
```

**Timestamp format**: `yyyy-mm-dd hh:nn:ss.zzz`
- ISO 8601 compatible date format (sortable)
- 24-hour time format  
- Millisecond precision for performance analysis
- Consistent across console and file output

### ⚙️ Configuration
- **Method chaining** for fluent configuration
- **Log level filtering** - set minimum level to reduce noise
- **File rotation** - automatic rotation when files exceed size limit
- **Custom file paths** with automatic directory creation

### 🌍 Cross-Platform
- **Windows**: Console colors via Windows API
- **Unix/Linux**: Console colors via ANSI escape codes
- **Consistent behavior** across platforms

## 📚 API Reference

### 🏭 Factory Methods
```pascal
TSimpleLog.Console              // Console output only
TSimpleLog.FileLog('file.log')  // File output only  
TSimpleLog.Both('file.log')     // Both console and file
```

### 🔊 Logging Methods
```pascal
Log.Debug('Debug message');
Log.Info('Information');
Log.Warning('Warning message');
Log.Error('Error occurred');
Log.Fatal('Critical error');

// Format string variants
Log.Info('User %s has %d items', [username, count]);
Log.Error('Connection failed: %s:%d', [host, port]);
```

### ⛓️ Configuration (Method Chaining)
```pascal
Log := TSimpleLog.Console
  .SetMinLevel(llWarning)           // Only warnings and above
  .SetOutputs([odConsole, odFile])  // Enable both outputs
  .SetFile('app.log')               // Set log file
  .SetMaxFileSize(5 * 1024 * 1024)  // 5MB rotation limit
  .SetSilent(False);                // Enable/disable all logging
```

### 🔧 Properties
```pascal
Log.Outputs := [odConsole, odFile];  // Set output destinations
Log.LogFile := 'myapp.log';          // Set log file path
Log.MinLevel := llInfo;              // Set minimum log level
Log.MaxFileSize := 10 * 1024 * 1024; // Set rotation size (10MB)
Log.Silent := True;                  // Enable/disable silent mode
```

## 📋 Examples

### 🌟 Real-World Usage Example

```pascal
program MyApplication;
uses SimpleLog;

var
  Log: TSimpleLog;
  UserCount: Integer;
begin
  // Initialize logging to both console and file
  Log := TSimpleLog.Both('application.log')
    .SetMinLevel(llInfo);  // Hide debug messages in production
  
  Log.Info('Application starting up');
  
  try
    // Your application logic
    UserCount := LoadUsers();
    Log.Info('Loaded %d users from database', [UserCount]);
    
    if UserCount = 0 then
      Log.Warning('No users found in database');
      
  except
    on E: Exception do
    begin
      Log.Error('Startup failed: %s', [E.Message]);
      ExitCode := 1;
      Exit;
    end;
  end;
  
  Log.Info('Application ready');
  
  // Your main application loop here...
  
  Log.Info('Application shutting down');
end;
```

### 🔍 Level Filtering
```pascal
// Production: Only show warnings and errors
Log := TSimpleLog.Both('prod.log').SetMinLevel(llWarning);

// Development: Show everything including debug
Log := TSimpleLog.Console.SetMinLevel(llDebug);
```

### 🔄 File Rotation
```pascal
// Rotate when file exceeds 1MB
Log := TSimpleLog.FileLog('big.log').SetMaxFileSize(1024 * 1024);
Log.Info('This will rotate when file gets too big');
```

### 🔇 Silent Mode
```pascal
// Temporarily disable all logging
Log.SetSilent(True);
Log.Error('This error will not appear anywhere');

// Re-enable logging
Log.SetSilent(False);
Log.Info('Logging is back on');
```

### 🧵 Thread Safety
```pascal
// SimpleLog is thread-safe by default!
// All logging operations are protected by a critical section.
// You can safely use the same logger instance from multiple threads.
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Both('threaded.log');
  // Use Log from multiple threads with confidence
  Log.Info('Multi-threaded logging works reliably');
end;
```

## 💯 Advanced Record Benefits

SimpleLog uses Free Pascal's **advanced records** instead of classes:

- ✅ **No memory management** - no Create/Free needed
- ✅ **Stack allocated** - automatic cleanup
- ✅ **Lightweight** - minimal overhead
- ✅ **Value semantics** - can be copied safely
- ✅ **Modern Pascal** - leverages language features

## 📁 File Structure

```text
SimpleLog-FP/
├── src/
│   └── SimpleLog.pas          # Main library (~415 lines)
├── examples/
│   ├── SimpleLogExample/      # Basic usage examples
│   └── ThreadSafeExample/     # Concurrent logging demo
├── tests/
│   └── SimpleLog.Test.pas     # Comprehensive test suite
├── docs/
│   ├── SimpleLogger.md        # User manual
│   └── cheat-sheet.md         # Quick API reference
└── README.md                  # This file
```

## 🧪 Tests

A comprehensive test suite is provided in the `tests/` directory. To run the tests:

- Open `TestRunner.lpi` in the Lazarus IDE or use `lazbuild` (required)
- Build and run the project to execute all tests
- Review the output in the IDE or generated log files

## 📦 Lazarus Package

A ready-to-use Lazarus package is included for easy integration:

- Open `packages/lazarus/simplelog.lpk` in the Lazarus IDE
- Click "Use" → "Add to Project" to add SimpleLog-FP to your project
- The package will automatically add the correct search paths

This is the recommended way to add SimpleLog-FP to your Lazarus projects for the best experience.

## 📊 Comparison with Complex Logging Libraries

| Feature | SimpleLog-FP | Complex Logger |
|---------|--------------|----------------|
| Lines of code | ~415 | 2000+ |
| Learning curve | Minutes | Hours |
| Features | 3 core outputs | 20+ features |
| Maintenance | Easy | Complex |
| Memory usage | Minimal | Higher |
| Dependencies | None | Multiple |

## 🤝 Contributing

Keep it simple! Any contributions should maintain the core philosophy:

- No feature bloat
- Easy to understand
- Focused on core logging needs

## ⚖️ License

MIT License - see [LICENSE](LICENSE.md) file for details.

## 🙏 Acknowledgments

- [Free Pascal Dev Team](https://www.freepascal.org/) for the Pascal compiler
- [Lazarus IDE Team](https://www.lazarus-ide.org/) for such an amazing IDE
- The kind and helpful individuals on various online platforms such as:
    - [Unofficial Free Pascal discord server](https://discord.com/channels/570025060312547359/570091337173696513)
    - [Free Pascal & Lazarus forum](https://forum.lazarus.freepascal.org/index.php)
    - [Tweaking4All Delphi, Lazarus, Free Pascal forum](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/)
    - [Laz Planet - Blogspot](https://lazplanet.blogspot.com/) / [Laz Planet - GitLab](https://lazplanet.gitlab.io/)
    - [Delphi Basics](https://www.delphibasics.co.uk/index.html)
    - @gcarreno for various suggestions and improvements
- All contributors who have helped improve this project
