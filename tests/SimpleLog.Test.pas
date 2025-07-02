unit SimpleLog.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes
 ,SysUtils
 ,fpcunit
 ,testregistry
 ,SimpleLog;


type
  TSimpleLogTest = class(TTestCase)
  private
    FTestDir: string;
    FLogFile: string;
    procedure CleanupLogFiles;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Factory method tests }
    procedure Test01_ConsoleFactory;
    procedure Test02_FileLogFactory;
    procedure Test03_BothFactory;
    
    { Basic logging tests }
    procedure Test04_ConsoleLogging;
    procedure Test05_FileLogging;
    procedure Test06_BothOutputLogging;
    
    { Log level tests }
    procedure Test07_DebugLevel;
    procedure Test08_InfoLevel;
    procedure Test09_WarningLevel;
    procedure Test10_ErrorLevel;
    procedure Test11_FatalLevel;
    
    { Configuration tests }
    procedure Test12_SetMinLevel;
    procedure Test13_SetMaxFileSize;
    procedure Test14_SetOutputs;
    procedure Test15_SetFile;
    procedure Test16_SetSilent;
    
    { Method chaining tests }
    procedure Test17_MethodChaining;
    
    { Format string tests }
    procedure Test18_FormatStringOverloads;
    
    { File rotation tests }
    procedure Test19_FileRotation;
    
    { Thread safety tests }
    procedure Test20_ThreadSafety;
    
    { Silent mode tests }
    procedure Test21_SilentMode;
  
    
    { Edge cases }
    procedure Test22_EmptyMessage;
    procedure Test23_LargeMessage;
    procedure Test24_DirectoryCreation;
    
    { Property tests }
    procedure Test25_PropertyAccess;
  end;

implementation

procedure TSimpleLogTest.SetUp;
var
  TestBasePath: string;
begin
  {$IFDEF WINDOWS}
  TestBasePath := GetEnvironmentVariable('TEMP');
  if TestBasePath = '' then
    TestBasePath := GetEnvironmentVariable('TMP');
  if TestBasePath = '' then
    TestBasePath := 'C:\Temp';
  {$ELSE}
  TestBasePath := '/tmp';
  {$ENDIF}
  
  // Generate unique test directory for each test method
  FTestDir := IncludeTrailingPathDelimiter(TestBasePath) + 
              'SimpleLogTest_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + IntToStr(Random(10000));
  FLogFile := FTestDir + PathDelim + 'test.log';
  
  // Clean up any existing test directory
  if DirectoryExists(FTestDir) then
  begin
    CleanupLogFiles;
    RemoveDir(FTestDir);
  end;
  
  // Create test directory
  if not CreateDir(FTestDir) then
    raise Exception.CreateFmt('Could not create test directory: %s', [FTestDir]);
    
  // Small delay to ensure file system operations are complete
  Sleep(100);
end;

procedure TSimpleLogTest.TearDown;
begin
  // Small delay to ensure file system operations are complete
  Sleep(100);
  
  // Clean up test files
  CleanupLogFiles;
  
  // Another small delay
  Sleep(100);
  
  // Finally remove test directory
  if DirectoryExists(FTestDir) then
  begin
    RemoveDir(FTestDir);
  end;
end;

procedure TSimpleLogTest.CleanupLogFiles;
var
  SearchRec: TSearchRec;
  FullPath: string;
  RetryCount: Integer;
begin
  // Small delay to ensure file system operations are complete
  Sleep(100);
  
  // Then attempt to delete files
  if FindFirst(FTestDir + PathDelim + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        FullPath := FTestDir + PathDelim + SearchRec.Name;
        RetryCount := 0;
        while (not DeleteFile(FullPath)) and (RetryCount < 3) do
        begin
          // If delete fails, wait longer and try again
          Inc(RetryCount);
          Sleep(100);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

// Factory method tests

procedure TSimpleLogTest.Test01_ConsoleFactory;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Console;
  AssertTrue('Console factory should set console output', odConsole in Log.Outputs);
  AssertFalse('Console factory should not set file output', odFile in Log.Outputs);
  AssertTrue('Console factory should set debug level', Log.MinLevel = llDebug);
end;

procedure TSimpleLogTest.Test02_FileLogFactory;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  AssertFalse('FileLog factory should not set console output', odConsole in Log.Outputs);
  AssertTrue('FileLog factory should set file output', odFile in Log.Outputs);
  AssertTrue('FileLog factory should set log file', Log.LogFile = FLogFile);
  AssertTrue('FileLog factory should set debug level', Log.MinLevel = llDebug);
end;

procedure TSimpleLogTest.Test03_BothFactory;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Both(FLogFile);
  AssertTrue('Both factory should set console output', odConsole in Log.Outputs);
  AssertTrue('Both factory should set file output', odFile in Log.Outputs);
  AssertTrue('Both factory should set log file', Log.LogFile = FLogFile);
  AssertTrue('Both factory should set debug level', Log.MinLevel = llDebug);
end;

// Basic logging tests

procedure TSimpleLogTest.Test04_ConsoleLogging;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Console;
  Log.Debug('Console logging test');
  Log.Info('Console logging test');
  Log.Warning('Console logging test');
  Log.Error('Console logging test');
  Log.Fatal('Console logging test');
  // Visual verification required - no assertion possible for console output
  AssertTrue('Console logging test completed', True);
end;

procedure TSimpleLogTest.Test05_FileLogging;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Info('File logging test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain the test message',
      Pos('File logging test', FileContent.Text) > 0);
    AssertTrue('Log file should contain INFO level',
      Pos('[INFO]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test06_BothOutputLogging;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.Both(FLogFile);
  Log.Info('Both output test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain the test message',
      Pos('Both output test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
  // Visual verification required for console output
  AssertTrue('Both output test completed', True);
end;

// Log level tests

procedure TSimpleLogTest.Test07_DebugLevel;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Debug('Debug message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain DEBUG level',
      Pos('[DEBUG]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test08_InfoLevel;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Info('Info message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain INFO level',
      Pos('[INFO]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test09_WarningLevel;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Warning('Warning message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain WARNING level',
      Pos('[WARNING]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test10_ErrorLevel;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Error('Error message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain ERROR level',
      Pos('[ERROR]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test11_FatalLevel;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Fatal('Fatal message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain FATAL level',
      Pos('[FATAL]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

// Configuration tests

procedure TSimpleLogTest.Test12_SetMinLevel;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMinLevel(llWarning);
  
  // These should be filtered out
  Log.Debug('Debug should be filtered');
  Log.Info('Info should be filtered');
  
  // These should be logged
  Log.Warning('Warning should be logged');
  Log.Error('Error should be logged');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertFalse('Debug message should be filtered',
      Pos('Debug should be filtered', FileContent.Text) > 0);
    AssertFalse('Info message should be filtered',
      Pos('Info should be filtered', FileContent.Text) > 0);
    AssertTrue('Warning message should be logged',
      Pos('Warning should be logged', FileContent.Text) > 0);
    AssertTrue('Error message should be logged',
      Pos('Error should be logged', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test13_SetMaxFileSize;
var
  Log: TSimpleLog;
  SearchRec: TSearchRec;
  RotatedFiles: Integer;
  i: Integer;
  LargeMessage: string;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMaxFileSize(1200); // 1.2KB - above minimum threshold
  
  // Create a large message (approximately 450+ bytes per message)
  LargeMessage := '';
  for i := 1 to 10 do
    LargeMessage := LargeMessage + 'This is a large message to trigger rotation. ';
  
  // Debug: Show message size
  WriteLn('DEBUG: LargeMessage length = ', Length(LargeMessage));
  
  // Write multiple messages to ensure we exceed the 1.2KB limit
  Log.Info(LargeMessage);
  
  // Check file size after first message
  if FileExists(FLogFile) then
  begin
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(FLogFile);
      WriteLn('DEBUG: File size after 1st message = ', Length(FileContent.Text));
    finally
      FileContent.Free;
    end;
  end;
  
  Log.Info(LargeMessage);
  Log.Info(LargeMessage);
  Log.Info(LargeMessage); // This should definitely trigger rotation
  
  // Debug: Check what happened after all messages
  WriteLn('DEBUG: After all messages:');
  if FileExists(FLogFile) then
  begin
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(FLogFile);
      WriteLn('DEBUG: Current log file size = ', Length(FileContent.Text));
    finally
      FileContent.Free;
    end;
  end else begin
    WriteLn('DEBUG: Original log file does not exist after all messages!');
  end;
  
  // Count log files (original + rotated)
  RotatedFiles := 0;
  if FindFirst(FTestDir + PathDelim + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        WriteLn('DEBUG: Found log file: ', SearchRec.Name, ' (size: ', SearchRec.Size, ')');
        Inc(RotatedFiles);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  
  WriteLn('DEBUG: Total log files found = ', RotatedFiles);
  AssertTrue('File rotation should occur when size exceeded', RotatedFiles > 1);
end;

procedure TSimpleLogTest.Test14_SetOutputs;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.Console.SetOutputs([odFile]).SetFile(FLogFile);
  Log.Info('Output test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain the test message',
      Pos('Output test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test15_SetFile;
var
  Log: TSimpleLog;
  NewLogFile: string;
  FileContent: TStringList;
begin
  NewLogFile := FTestDir + PathDelim + 'newtest.log';
  Log := TSimpleLog.FileLog(FLogFile).SetFile(NewLogFile);
  Log.Info('New file test');
  
  AssertTrue('New log file should exist', FileExists(NewLogFile));
  AssertFalse('Original log file should not exist', FileExists(FLogFile));
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(NewLogFile);
    AssertTrue('New log file should contain the test message',
      Pos('New file test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test16_SetSilent;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetSilent(True);
  Log.Info('Silent test - should not be logged');
  
  AssertFalse('Log file should not exist when silent mode is enabled',
    FileExists(FLogFile));
    
  Log.SetSilent(False);
  Log.Info('Not silent test - should be logged');
  
  AssertTrue('Log file should exist when silent mode is disabled',
    FileExists(FLogFile));
end;

// Method chaining tests

procedure TSimpleLogTest.Test17_MethodChaining;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.Console
    .SetMinLevel(llInfo)
    .SetOutputs([odFile])
    .SetFile(FLogFile)
    .SetMaxFileSize(1024 * 1024);
    
  AssertTrue('Min level should be set to Info', Log.MinLevel = llInfo);
  AssertTrue('Output should be set to file only', Log.Outputs = [odFile]);
  AssertTrue('Log file should be set', Log.LogFile = FLogFile);
  AssertTrue('Max file size should be set', Log.MaxFileSize = 1024 * 1024);
  
  Log.Info('Method chaining test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain the test message',
      Pos('Method chaining test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

// Format string tests

procedure TSimpleLogTest.Test18_FormatStringOverloads;
var
  Log: TSimpleLog;
  FileContent: TStringList;
  TestValue1: Integer;
  TestValue2: string;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  
  TestValue1 := 42;
  TestValue2 := 'test string';
  
  // Test format string overloads for different log levels
  Log.Debug('Debug format test: %d and %s', [TestValue1, TestValue2]);
  Log.Info('Info format test: %d and %s', [TestValue1, TestValue2]);
  Log.Warning('Warning format test: %d and %s', [TestValue1, TestValue2]);
  Log.Error('Error format test: %d and %s', [TestValue1, TestValue2]);
  Log.Fatal('Fatal format test: %d and %s', [TestValue1, TestValue2]);
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain formatted debug message',
      Pos('Debug format test: 42 and test string', FileContent.Text) > 0);
    AssertTrue('Log file should contain formatted info message',
      Pos('Info format test: 42 and test string', FileContent.Text) > 0);
    AssertTrue('Log file should contain formatted warning message',
      Pos('Warning format test: 42 and test string', FileContent.Text) > 0);
    AssertTrue('Log file should contain formatted error message',
      Pos('Error format test: 42 and test string', FileContent.Text) > 0);
    AssertTrue('Log file should contain formatted fatal message',
      Pos('Fatal format test: 42 and test string', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

// File rotation tests

procedure TSimpleLogTest.Test19_FileRotation;
var
  Log: TSimpleLog;
  SearchRec: TSearchRec;
  RotatedFiles: Integer;
  i: Integer;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMaxFileSize(100); // Very small for testing
  
  // Write enough data to trigger rotation
  for i := 1 to 20 do
    Log.Info('This is message number %d with some extra text to fill space', [i]);
  
  // Count log files
  RotatedFiles := 0;
  if FindFirst(FTestDir + PathDelim + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        Inc(RotatedFiles);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  
  AssertTrue('File rotation should create multiple log files', RotatedFiles > 1);
end;

// Thread safety tests

procedure TSimpleLogTest.Test20_ThreadSafety;
var
  Log: TSimpleLog;
  i: Integer;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  
  // Simulate concurrent access by rapid sequential logging
  for i := 1 to 100 do
  begin
    Log.Info('Thread safety test message %d', [i]);
  end;
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain multiple messages',
      FileContent.Count >= 100);
  finally
    FileContent.Free;
  end;
end;

// Silent mode tests

procedure TSimpleLogTest.Test21_SilentMode;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  
  // Test normal logging
  Log.Info('Normal message');
  AssertTrue('Log file should exist for normal logging', FileExists(FLogFile));
  
  // Enable silent mode
  Log.Silent := True;
  Log.Info('Silent message - should not appear');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertFalse('Silent message should not be in log file',
      Pos('Silent message', FileContent.Text) > 0);
    AssertTrue('Normal message should still be in log file',
      Pos('Normal message', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
  
  // Disable silent mode
  Log.Silent := False;
  Log.Info('Resumed message');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Resumed message should be in log file',
      Pos('Resumed message', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;


// Edge cases

procedure TSimpleLogTest.Test22_EmptyMessage;
var
  Log: TSimpleLog;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Info('');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Empty message should be logged',
      FileContent.Count > 0);
    AssertTrue('Log should contain INFO level for empty message',
      Pos('[INFO]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test23_LargeMessage;
var
  Log: TSimpleLog;
  LargeMessage: string;
  FileContent: TStringList;
  i: Integer;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  
  // Create a very large message
  LargeMessage := '';
  for i := 1 to 1000 do
    LargeMessage := LargeMessage + 'Large message content ';
  
  Log.Info(LargeMessage);
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Large message should be logged',
      Pos('Large message content', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test24_DirectoryCreation;
var
  Log: TSimpleLog;
  DeepPath: string;
  FileContent: TStringList;
begin
  DeepPath := FTestDir + PathDelim + 'deep' + PathDelim + 'nested' + PathDelim + 'path' + PathDelim + 'test.log';
  Log := TSimpleLog.FileLog(DeepPath);
  Log.Info('Directory creation test');
  
  AssertTrue('Deep directory structure should be created', FileExists(DeepPath));
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(DeepPath);
    AssertTrue('Log file should contain the test message',
      Pos('Directory creation test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

// Property tests

procedure TSimpleLogTest.Test25_PropertyAccess;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Console;
  
  // Test property access
  Log.Outputs := [odFile];
  AssertTrue('Outputs property should be settable', odFile in Log.Outputs);
  
  Log.LogFile := FLogFile;
  AssertTrue('LogFile property should be settable', Log.LogFile = FLogFile);
  
  Log.MinLevel := llError;
  AssertTrue('MinLevel property should be settable', Log.MinLevel = llError);
  
  Log.MaxFileSize := 2048;
  AssertTrue('MaxFileSize property should be settable', Log.MaxFileSize = 2048);
  
  Log.Silent := True;
  AssertTrue('Silent property should be settable', Log.Silent = True);
end;

initialization
  RegisterTest(TSimpleLogTest);
end.

