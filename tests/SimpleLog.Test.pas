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
    procedure AssertLogContains(const AFileName, AExpected: string);
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
    
    { Configuration readback tests }
    procedure Test25_ConfigurationReadback;

    { Format filtering tests }
    procedure Test26_FilteredFormatStringDoesNotFormat;

    { Additional behavior tests }
    procedure Test27_FormattedOverloadWithLevelFiltering;
    procedure Test28_MinFileSizeClamp;
    procedure Test29_LogFormatShape;
    procedure Test30_InvalidPathDoesNotRaise;
    procedure Test31_RotationReplacesBoundedBackup;
    procedure Test32_UseColorsCanBeDisabled;
  end;

implementation

type
  TLogWorkerThread = class(TThread)
  private
    FLogger: TSimpleLog;
    FMessageCount: Integer;
    FThreadIndex: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ALogger: TSimpleLog; AThreadIndex, AMessageCount: Integer);
  end;

constructor TLogWorkerThread.Create(ALogger: TSimpleLog; AThreadIndex, AMessageCount: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FLogger := ALogger;
  FThreadIndex := AThreadIndex;
  FMessageCount := AMessageCount;
  Start;
end;

procedure TLogWorkerThread.Execute;
var
  i: Integer;
begin
  for i := 1 to FMessageCount do
    FLogger.Info('Thread %d message %d', [FThreadIndex, i]);
end;

procedure DeleteDirectoryTree(const ADir: string);
var
  SearchRec: TSearchRec;
  ItemPath: string;
begin
  if not DirectoryExists(ADir) then
    Exit;

  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        ItemPath := IncludeTrailingPathDelimiter(ADir) + SearchRec.Name;
        if (SearchRec.Attr and faDirectory) <> 0 then
          DeleteDirectoryTree(ItemPath)
        else
          DeleteFile(ItemPath);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;

  RemoveDir(ADir);
end;

function CountMatchingFiles(const APattern: string): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(APattern, faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        Inc(Result);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function ReadTextFile(const AFileName: string): string;
var
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(AFileName);
    Result := FileContent.Text;
  finally
    FileContent.Free;
  end;
end;

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
    CleanupLogFiles;
  
  // Create test directory
  if not CreateDir(FTestDir) then
    raise Exception.CreateFmt('Could not create test directory: %s', [FTestDir]);
end;

procedure TSimpleLogTest.TearDown;
begin
  CleanupLogFiles;
end;

procedure TSimpleLogTest.CleanupLogFiles;
begin
  DeleteDirectoryTree(FTestDir);
end;

procedure TSimpleLogTest.AssertLogContains(const AFileName, AExpected: string);
begin
  AssertTrue(Format('Log file should contain "%s"', [AExpected]),
    Pos(AExpected, ReadTextFile(AFileName)) > 0);
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
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMaxFileSize(1200); // 1.2KB - above minimum threshold
  
  // Create a large message (approximately 450+ bytes per message)
  LargeMessage := '';
  for i := 1 to 10 do
    LargeMessage := LargeMessage + 'This is a large message to trigger rotation. ';
  
  // Write multiple messages to ensure we exceed the 1.2KB limit
  Log.Info(LargeMessage);
  Log.Info(LargeMessage);
  Log.Info(LargeMessage);
  Log.Info(LargeMessage); // This should definitely trigger rotation
  
  // Count log files (original + rotated)
  RotatedFiles := 0;
  if FindFirst(FLogFile + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        Inc(RotatedFiles);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  
  AssertTrue('File rotation should create a backup file', FileExists(FLogFile + '.1'));
  AssertTrue('File rotation should keep one bounded backup', RotatedFiles <= 2);
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
    
  Log := Log.SetSilent(False);
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
  if FindFirst(FLogFile + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        Inc(RotatedFiles);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  
  AssertTrue('File rotation should create one backup file', FileExists(FLogFile + '.1'));
  AssertTrue('File rotation should keep one bounded backup', RotatedFiles <= 2);
end;

// Thread safety tests

procedure TSimpleLogTest.Test20_ThreadSafety;
const
  THREAD_COUNT = 4;
  MESSAGES_PER_THREAD = 50;
var
  Log: TSimpleLog;
  i: Integer;
  Threads: array[1..THREAD_COUNT] of TLogWorkerThread;
  FileContent: TStringList;
begin
  Log := TSimpleLog.FileLog(FLogFile);
  
  for i := 1 to THREAD_COUNT do
    Threads[i] := TLogWorkerThread.Create(Log, i, MESSAGES_PER_THREAD);

  for i := 1 to THREAD_COUNT do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertEquals('Log file should contain every threaded message',
      THREAD_COUNT * MESSAGES_PER_THREAD, FileContent.Count);
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
  Log := Log.SetSilent(True);
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
  Log := Log.SetSilent(False);
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

procedure TSimpleLogTest.Test25_ConfigurationReadback;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Console
    .SetOutputs([odFile])
    .SetFile(FLogFile)
    .SetMinLevel(llError)
    .SetMaxFileSize(2048)
    .SetSilent(True)
    .SetUseColors(False);

  AssertTrue('Outputs property should reflect configured outputs', Log.Outputs = [odFile]);
  AssertTrue('LogFile property should reflect configured file', Log.LogFile = FLogFile);
  AssertTrue('MinLevel property should reflect configured level', Log.MinLevel = llError);
  AssertTrue('MaxFileSize property should reflect configured size', Log.MaxFileSize = 2048);
  AssertTrue('Silent property should reflect configured silent mode', Log.Silent);
  AssertFalse('UseColors property should reflect configured color mode', Log.UseColors);
end;

procedure TSimpleLogTest.Test26_FilteredFormatStringDoesNotFormat;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMinLevel(llFatal);
  Log.Info('Filtered value: %d', ['not an integer']);
  AssertFalse('Filtered formatted message should not create a log file', FileExists(FLogFile));

  Log := TSimpleLog.FileLog(FLogFile).SetSilent(True);
  Log.Fatal('Silent value: %d', ['not an integer']);
  AssertFalse('Silent formatted message should not create a log file', FileExists(FLogFile));
end;

procedure TSimpleLogTest.Test27_FormattedOverloadWithLevelFiltering;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMinLevel(llWarning);
  Log.Info('Filtered %s %d', ['format', 1]);
  Log.Warning('Visible %s %d', ['format', 2]);

  AssertLogContains(FLogFile, '[WARNING]');
  AssertLogContains(FLogFile, 'Visible format 2');
  AssertFalse('Filtered formatted overload should not be logged',
    Pos('Filtered format 1', ReadTextFile(FLogFile)) > 0);
end;

procedure TSimpleLogTest.Test28_MinFileSizeClamp;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMaxFileSize(1);
  AssertTrue('SetMaxFileSize should clamp very small values to 1KB',
    Log.MaxFileSize = 1024);
end;

procedure TSimpleLogTest.Test29_LogFormatShape;
var
  Log: TSimpleLog;
  FileContent: TStringList;
  Line: string;

  function IsDigitAt(AIndex: Integer): Boolean;
  begin
    Result := (Length(Line) >= AIndex) and (Line[AIndex] in ['0'..'9']);
  end;

begin
  Log := TSimpleLog.FileLog(FLogFile);
  Log.Info('Shape test');

  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain one line', FileContent.Count >= 1);
    Line := FileContent[0];

    AssertTrue('Log line should contain the expected message suffix',
      Pos('] [INFO] Shape test', Line) > 0);
    AssertTrue('Log line should start with [', (Length(Line) >= 1) and (Line[1] = '['));
    AssertTrue('Log timestamp should contain a 4 digit year',
      IsDigitAt(2) and IsDigitAt(3) and IsDigitAt(4) and IsDigitAt(5));
    AssertTrue('Log timestamp should use yyyy-mm-dd date separators',
      (Length(Line) >= 11) and (Line[6] = '-') and (Line[9] = '-'));
    AssertTrue('Log timestamp should use hh:nn:ss.zzz time separators',
      (Length(Line) >= 25) and (Line[12] = ' ') and (Line[15] = ':') and
      (Line[18] = ':') and (Line[21] = '.') and (Line[25] = ']'));
  finally
    FileContent.Free;
  end;
end;

procedure TSimpleLogTest.Test30_InvalidPathDoesNotRaise;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.FileLog(FTestDir);
  Log.Info('This attempts to write to a directory path');
  AssertTrue('Invalid file target should not remove the test directory',
    DirectoryExists(FTestDir));
end;

procedure TSimpleLogTest.Test31_RotationReplacesBoundedBackup;
var
  Log: TSimpleLog;
  FirstBackup: TStringList;
  SecondBackup: TStringList;
  LargeMessage1: string;
  LargeMessage2: string;
  FirstBackupText: string;
  i: Integer;
begin
  Log := TSimpleLog.FileLog(FLogFile).SetMaxFileSize(1024);

  LargeMessage1 := '';
  LargeMessage2 := '';
  for i := 1 to 80 do
  begin
    LargeMessage1 := LargeMessage1 + 'first rotation payload ';
    LargeMessage2 := LargeMessage2 + 'second rotation payload ';
  end;

  Log.Info(LargeMessage1);
  Log.Info('create first backup');

  AssertTrue('First rotation should create bounded backup', FileExists(FLogFile + '.1'));
  FirstBackup := TStringList.Create;
  try
    FirstBackup.LoadFromFile(FLogFile + '.1');
    FirstBackupText := FirstBackup.Text;
  finally
    FirstBackup.Free;
  end;

  Log.Info(LargeMessage2);
  Log.Info('create second backup');

  AssertTrue('Second rotation should keep bounded backup', FileExists(FLogFile + '.1'));
  AssertTrue('Rotation should keep only current file and one backup',
    CountMatchingFiles(FLogFile + '*') <= 2);

  SecondBackup := TStringList.Create;
  try
    SecondBackup.LoadFromFile(FLogFile + '.1');
    AssertFalse('Second rotation should replace the previous backup',
      SecondBackup.Text = FirstBackupText);
  finally
    SecondBackup.Free;
  end;
end;

procedure TSimpleLogTest.Test32_UseColorsCanBeDisabled;
var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Console.SetUseColors(False);
  AssertFalse('UseColors should be disabled', Log.UseColors);
  Log.Info('Color disabled console smoke test');

  Log := Log.SetUseColors(True);
  AssertTrue('UseColors should be enabled', Log.UseColors);
end;

initialization
  RegisterTest(TSimpleLogTest);
end.

