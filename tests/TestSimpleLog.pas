program TestSimpleLog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, SimpleLog;

procedure TestBasicFunctionality;
var
  Log: TSimpleLog;
begin
  WriteLn('Testing basic functionality...');
  
  // Test console logging
  Log := TSimpleLog.Console;
  Log.Info('Console test message');
  
  // Test file logging
  Log := TSimpleLog.FileLog('test.log');
  Log.Info('File test message');
  
  // Test both
  Log := TSimpleLog.Both('both.log');
  Log.Info('Both console and file message');
  
  WriteLn('✓ Basic functionality tests passed');
end;

procedure TestLogLevels;
var
  Log: TSimpleLog;
begin
  WriteLn('Testing log levels...');
  
  Log := TSimpleLog.Console;
  
  // Test all levels
  Log.Debug('Debug level');
  Log.Info('Info level');
  Log.Warning('Warning level');
  Log.Error('Error level');
  Log.Fatal('Fatal level');
  
  WriteLn('✓ Log level tests passed');
end;

procedure TestFiltering;
var
  Log: TSimpleLog;
begin
  WriteLn('Testing level filtering...');
  
  Log := TSimpleLog.Console.SetMinLevel(llWarning);
  
  WriteLn('Should see only Warning, Error, Fatal:');
  Log.Debug('This should NOT appear');
  Log.Info('This should NOT appear');
  Log.Warning('This SHOULD appear');
  Log.Error('This SHOULD appear');
  Log.Fatal('This SHOULD appear');
  
  WriteLn('✓ Filtering tests passed');
end;

procedure TestFormatStrings;
var
  Log: TSimpleLog;
begin
  WriteLn('Testing format strings...');
  
  Log := TSimpleLog.Console;
  
  Log.Info('String: %s, Integer: %d, Float: %.2f', ['test', 42, 3.14159]);
  Log.Warning('Multiple values: %s=%d, %s=%.1f', ['count', 10, 'percentage', 85.5]);
  
  WriteLn('✓ Format string tests passed');
end;

procedure TestMethodChaining;
var
  Log: TSimpleLog;
begin
  WriteLn('Testing method chaining...');
  
  Log := TSimpleLog.Console
    .SetMinLevel(llInfo)
    .SetFile('chained_test.log')
    .SetOutputs([odConsole, odFile]);
    
  Log.Info('Method chaining works!');
  
  WriteLn('✓ Method chaining tests passed');
end;

procedure TestArchivingLogFiles;
var
  Log: TSimpleLog;
  i : Word;
begin
  WriteLn('Testing archiving log files...');

  Log := TSimpleLog.Console
    .SetMinLevel(llInfo)
    .SetFile('archiving.log')
    .SetOutputs([odConsole, odFile]);
    .SetMaxFileSize(1 * 512 * 512);
  Log.ArchiveLogFiles := True;
  Log.MaxRotateFiles := 3;

  for i := 0 to 20000 do
    Log.Info('Archiving log files works!');

  WriteLn('✓ Archiving log files tests passed');
end;

begin
  WriteLn('=== SimpleLog-FP Test Suite ===');
  WriteLn;
  
  try
    TestBasicFunctionality;
    WriteLn;
    
    TestLogLevels;
    WriteLn;
    
    TestFiltering;
    WriteLn;
    
    TestFormatStrings;
    WriteLn;
    
    TestMethodChaining;
    WriteLn;

    TestArchivingLogFiles;
    WriteLn;
    
    WriteLn('🎉 All tests passed!');
    WriteLn('Check the created log files: test.log, both.log, chained_test.log');
    
  except
    on E: Exception do
    begin
      WriteLn('❌ Test failed: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
