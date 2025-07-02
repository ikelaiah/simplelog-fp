program SimpleLogExample;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleLog;

var
  Log: TSimpleLog;
  ConsoleOnly: TSimpleLog;
  FileOnly: TSimpleLog;

begin
  WriteLn('=== SimpleLog-FP Demo ===');
  WriteLn;
  
  // Example 1: Console logging only
  WriteLn('1. Console logging:');
  ConsoleOnly := TSimpleLog.Console;
  ConsoleOnly.Info('Hello from console!');
  ConsoleOnly.Warning('This is a warning');
  ConsoleOnly.Error('This is an error');
  WriteLn;
  
  // Example 2: File logging only
  WriteLn('2. File logging (check simple.log):');
  FileOnly := TSimpleLog.FileLog('simple.log');
  FileOnly.Info('This goes to file only');
  FileOnly.Debug('Debug message in file');
  WriteLn('Messages written to simple.log');
  WriteLn;
  
  // Example 3: Both console and file
  WriteLn('3. Both console and file:');
  Log := TSimpleLog.Both('app.log');
  Log.Info('This appears in both console and file');
  Log.Warning('Warning goes to both destinations');
  WriteLn;
  
  // Example 4: Method chaining configuration
  WriteLn('4. Method chaining:');
  Log := TSimpleLog.Console
    .SetMinLevel(llWarning)  // Only warnings and above
    .SetOutputs([odConsole, odFile])
    .SetFile('chained.log');
  Log.Debug('This debug message is filtered out');
  Log.Warning('This warning appears (level filter)');
  Log.Error('This error appears');
  WriteLn;
  
  // Example 5: Format strings
  WriteLn('5. Format strings:');
  Log := TSimpleLog.Console;
  Log.Info('User %s logged in with ID %d', ['John', 123]);
  Log.Warning('Memory usage: %.1f%%', [85.5]);
  Log.Error('Failed to connect to %s:%d', ['localhost', 8080]);
  WriteLn;
  
  // Example 6: Different log levels with colors
  WriteLn('6. All log levels (with colors):');
  Log := TSimpleLog.Console;
  Log.Debug('Debug: Detailed information for diagnosing problems');
  Log.Info('Info: General information about program execution');
  Log.Warning('Warning: Something unexpected happened, but we can continue');
  Log.Error('Error: A serious problem occurred');
  Log.Fatal('Fatal: Critical error, program should probably stop');
  WriteLn;
  
  WriteLn('Demo completed! Check the log files created.');
  
  // Cleanup - no memory management needed with advanced records!
end.
