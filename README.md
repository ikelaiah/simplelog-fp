# 📝 SimpleLog-FP 

[![License: MIT](https://img.shields.io/badge/License-MIT-4682b4.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-4e7ddc.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-6fa8dc.svg)](https://www.lazarus-ide.org/)
[![Platform](https://img.shields.io/badge/Platform-Windows%20%7C%20Linux-6aa84f.svg)](#)
[![Version](https://img.shields.io/badge/version-0.5.0-4682b4.svg)](CHANGELOG.md)
[![No Dependencies](https://img.shields.io/badge/dependencies-none-4e7ddc.svg)](#)


A **simple**, **lightweight**, and **easy-to-use** logging library for Free Pascal applications.

> **Why SimpleLog-FP?** 🤔 Born from the need for a logging library that's actually simple to use and maintain. No feature bloat, no complexity - just clean, reliable logging for console, file, or both.

## 🎯 Design Philosophy

- ✅ **Simple** - Easy for new developers to understand and use
- ✅ **Lightweight** - ~400 lines of code, no bloat
- ✅ **Maintainable** - Clean code that one person can easily maintain  
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
- **Thread safety** with file-level safety for multi-threaded applications
- **Silent mode** to temporarily disable all logging output
- **Flush method** for forcing immediate file writes

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

### 🛠️ Utility Methods
```pascal
Log.Flush;                           // Force immediate file write
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
// File operations are inherently safer in most file systems
// Multiple threads can safely use the same logger instance
// For high-concurrency scenarios, consider using separate logger instances
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Both('threaded.log');
  // Use Log from multiple threads with basic safety
  Log.Info('Multi-threaded logging works reliably');
  Log.Flush; // Force immediate write
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

```
SimpleLog-FP/
├── src/
│   └── SimpleLog.pas          # Main library (~500 lines)
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

## 📊 Comparison with Complex Logging Libraries

| Feature | SimpleLog-FP | Complex Logger |
|---------|--------------|----------------|
| Lines of code | ~400 | 2000+ |
| Learning curve | Minutes | Hours |
| Features | 3 core outputs | 20+ features |
| Maintenance | Easy | Complex |
| Memory usage | Minimal | Higher |
| Dependencies | None | Multiple |

## 🤝 Contributing

Keep it simple! Any contributions should maintain the core philosophy:

- No feature bloat
- Easy to understand
- Maintainable by one person
- Focused on core logging needs

## ⚖️ License

MIT License - see [LICENSE](LICENSE.md) file for details.

## 🙏 Acknowledgments

- Inspired by modern CLI frameworks
- Built with Free Pascal and Lazarus IDE
- Thanks to the Free Pascal community for their support and contributions
