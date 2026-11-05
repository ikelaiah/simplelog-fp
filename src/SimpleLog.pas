unit SimpleLog;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, Termio{$ENDIF};

type
  ESimpleLogError = class(Exception);

  { Log level enumeration }
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
  
  { Output destination enumeration }
  TOutputDestination = (odConsole, odFile);
  TOutputDestinations = set of TOutputDestination;

  { Console color behavior }
  TConsoleColorMode = (ccmAuto, ccmAlways, ccmNever);

  { Which part of a console line is colored }
  TConsoleColorScope = (ccsLevel, ccsLine);

  { Logging I/O error behavior }
  TLogErrorMode = (lemSilent, lemRaise);

  { Simple logging advanced record }
  TSimpleLog = record
  private
    FOutputs: TOutputDestinations;
    FLogFile: string;
    FMinLevel: TLogLevel;
    FMaxFileSize: Int64;
    FSilent: Boolean;
    FColorMode: TConsoleColorMode;
    FColorScope: TConsoleColorScope;
    FErrorMode: TLogErrorMode;
    FArchiveLogFiles: Boolean;
    FMaxRotateFiles: Integer;

    class function CreateLogger(AOutputs: TOutputDestinations; const AFileName: string): TSimpleLog; static;
    function ShouldLog(ALevel: TLogLevel): Boolean;
    function ShouldUseColors: Boolean;
    procedure WriteColored(const AText: string; ALevel: TLogLevel; ATagOnly: Boolean);
    procedure WriteToConsole(const APrefix, ALevelTag, ASuffix: string; ALevel: TLogLevel);
    procedure WriteToFile(const AMessage: string);
    procedure SetConsoleColor(ALevel: TLogLevel; ATagOnly: Boolean);
    function GetLogLevelStr(ALevel: TLogLevel): string;
    procedure LogFmt(ALevel: TLogLevel; const AFormat: string; const AArgs: array of const);
    procedure WriteLogEntry(ALevel: TLogLevel; const AMessage: string);
    procedure CheckFileRotation;
    procedure ArchiveActiveLog;
    function ZipActiveLog(const AZipFileName: string; out AErrorText: string): Boolean;
    function GetBackupFileName: string;
    function GetArchiveFileName(AIndex: Integer): string;
    function EnsureDirectoryExists(const ADir: string): Boolean;
    procedure HandleError(const AOperation, ADetails: string);
    function GetOutputs: TOutputDestinations;
    function GetLogFile: string;
    function GetMinLevel: TLogLevel;
    function GetMaxFileSize: Int64;
    function GetSilent: Boolean;
    function GetColorMode: TConsoleColorMode;
    function GetColorScope: TConsoleColorScope;
    function GetUseColors: Boolean;
    function GetErrorMode: TLogErrorMode;
    function GetArchiveLogFiles: Boolean;
    function GetMaxRotateFiles: Integer;
  public
    { Factory methods }
    class function Console: TSimpleLog; static;
    class function FileLog(const AFileName: string): TSimpleLog; static;
    class function Both(const AFileName: string): TSimpleLog; static;

    { Configuration }
    function SetOutputs(AOutputs: TOutputDestinations): TSimpleLog;
    function SetFile(const AFileName: string): TSimpleLog;
    function SetMinLevel(ALevel: TLogLevel): TSimpleLog;
    function SetMaxFileSize(ASize: Int64): TSimpleLog;
    function SetSilent(ASilent: Boolean): TSimpleLog;
    function SetColorMode(AMode: TConsoleColorMode): TSimpleLog;
    function SetUseColors(AUseColors: Boolean): TSimpleLog;
    function SetColorScope(AScope: TConsoleColorScope): TSimpleLog;
    function SetErrorMode(AMode: TLogErrorMode): TSimpleLog;
    function SetArchiveLogFiles(AArchive: Boolean): TSimpleLog;
    function SetMaxRotateFiles(ACount: Integer): TSimpleLog;

    { Logging methods }
    procedure Log(ALevel: TLogLevel; const AMessage: string);
    procedure Debug(const AMessage: string);
    procedure Info(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Fatal(const AMessage: string);

    { Format string overloads }
    procedure Debug(const AFormat: string; const AArgs: array of const); overload;
    procedure Info(const AFormat: string; const AArgs: array of const); overload;
    procedure Warning(const AFormat: string; const AArgs: array of const); overload;
    procedure Error(const AFormat: string; const AArgs: array of const); overload;
    procedure Fatal(const AFormat: string; const AArgs: array of const); overload;

    { Properties }
    property Outputs: TOutputDestinations read GetOutputs;
    property LogFile: string read GetLogFile;
    property MinLevel: TLogLevel read GetMinLevel;
    property MaxFileSize: Int64 read GetMaxFileSize;
    property Silent: Boolean read GetSilent;
    property ColorMode: TConsoleColorMode read GetColorMode;
    property ColorScope: TConsoleColorScope read GetColorScope;
    property UseColors: Boolean read GetUseColors;
    property ErrorMode: TLogErrorMode read GetErrorMode;
    property ArchiveLogFiles: Boolean read GetArchiveLogFiles;
    property MaxRotateFiles: Integer read GetMaxRotateFiles;
  end;

const
  DEFAULT_MAX_ROTATE_FILES = 10;
  MAX_ROTATE_FILES = 255;

implementation

uses
  Zipper;

const
  DEFAULT_MAX_FILE_SIZE = 10 * 1024 * 1024; // 10MB
  MIN_MAX_FILE_SIZE = 1024; // 1KB minimum to prevent I/O overload

var
  GLogLock: TRTLCriticalSection;

procedure EnterLogLock;
begin
  System.EnterCriticalSection(GLogLock);
end;

procedure LeaveLogLock;
begin
  System.LeaveCriticalSection(GLogLock);
end;

{ TSimpleLog }
class function TSimpleLog.CreateLogger(AOutputs: TOutputDestinations; const AFileName: string): TSimpleLog; static;
begin
  Result.FOutputs := AOutputs;
  Result.FLogFile := AFileName;
  Result.FMinLevel := llDebug;
  Result.FMaxFileSize := DEFAULT_MAX_FILE_SIZE;
  Result.FSilent := False;
  Result.FColorMode := ccmAuto;
  Result.FColorScope := ccsLevel;
  Result.FErrorMode := lemSilent;
  Result.FArchiveLogFiles := False;
  Result.FMaxRotateFiles := DEFAULT_MAX_ROTATE_FILES;
end;

class function TSimpleLog.Console: TSimpleLog; static;
begin
  Result := CreateLogger([odConsole], '');
end;

class function TSimpleLog.FileLog(const AFileName: string): TSimpleLog;
begin
  Result := CreateLogger([odFile], AFileName);
end;

class function TSimpleLog.Both(const AFileName: string): TSimpleLog;
begin
  Result := CreateLogger([odConsole, odFile], AFileName);
end;

function TSimpleLog.SetOutputs(AOutputs: TOutputDestinations): TSimpleLog;
begin
  EnterLogLock;
  try
    FOutputs := AOutputs;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetFile(const AFileName: string): TSimpleLog;
begin
  EnterLogLock;
  try
    FLogFile := AFileName;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetMinLevel(ALevel: TLogLevel): TSimpleLog;
begin
  EnterLogLock;
  try
    FMinLevel := ALevel;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetMaxFileSize(ASize: Int64): TSimpleLog;
begin
  EnterLogLock;
  try
    // Enforce minimum file size to prevent I/O overload
    if ASize < MIN_MAX_FILE_SIZE then
      FMaxFileSize := MIN_MAX_FILE_SIZE
    else
      FMaxFileSize := ASize;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetSilent(ASilent: Boolean): TSimpleLog;
begin
  EnterLogLock;
  try
    FSilent := ASilent;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetUseColors(AUseColors: Boolean): TSimpleLog;
begin
  if AUseColors then
    Result := SetColorMode(ccmAlways)
  else
    Result := SetColorMode(ccmNever);
end;

function TSimpleLog.SetColorMode(AMode: TConsoleColorMode): TSimpleLog;
begin
  EnterLogLock;
  try
    FColorMode := AMode;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetErrorMode(AMode: TLogErrorMode): TSimpleLog;
begin
  EnterLogLock;
  try
    FErrorMode := AMode;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetColorScope(AScope: TConsoleColorScope): TSimpleLog;
begin
  EnterLogLock;
  try
    FColorScope := AScope;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetArchiveLogFiles(AArchive: Boolean): TSimpleLog;
begin
  EnterLogLock;
  try
    FArchiveLogFiles := AArchive;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.SetMaxRotateFiles(ACount: Integer): TSimpleLog;
begin
  EnterLogLock;
  try
    if ACount < 1 then
      FMaxRotateFiles := 1
    else if ACount > MAX_ROTATE_FILES then
      FMaxRotateFiles := MAX_ROTATE_FILES
    else
      FMaxRotateFiles := ACount;
    Result := Self;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.ShouldLog(ALevel: TLogLevel): Boolean;
begin
  Result := (not FSilent) and (ALevel >= FMinLevel);
end;

function TSimpleLog.ShouldUseColors: Boolean;
{$IFDEF WINDOWS}
var
  ConsoleMode: DWORD;
  ConsoleHandle: THandle;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  ConsoleMode := 0;
  {$ENDIF}
  case FColorMode of
    ccmAlways:
      Result := True;
    ccmNever:
      Result := False;
    ccmAuto:
      begin
        {$IFDEF WINDOWS}
        ConsoleHandle := THandle(TextRec(Output).Handle);
        Result := (ConsoleHandle <> INVALID_HANDLE_VALUE) and
          GetConsoleMode(ConsoleHandle, ConsoleMode);
        {$ELSE}
          {$IFDEF UNIX}
          Result := IsATTY(Output) = 1;
          {$ELSE}
          Result := False;
          {$ENDIF}
        {$ENDIF}
      end;
  else
    Result := False;
  end;
end;

procedure TSimpleLog.SetConsoleColor(ALevel: TLogLevel; ATagOnly: Boolean);
{$IFDEF WINDOWS}
var
  ConsoleHandle: THandle;
{$ENDIF}
begin
  // INFO is blue only on the level tag; whole INFO lines keep the default color.
  {$IFDEF WINDOWS}
  ConsoleHandle := THandle(TextRec(Output).Handle);
  case ALevel of
    llDebug: SetConsoleTextAttribute(ConsoleHandle, 8);    // Gray
    llInfo:
      if ATagOnly then
        SetConsoleTextAttribute(ConsoleHandle, 9)          // Blue
      else
        SetConsoleTextAttribute(ConsoleHandle, 7);         // White
    llWarning: SetConsoleTextAttribute(ConsoleHandle, 14); // Yellow
    llError: SetConsoleTextAttribute(ConsoleHandle, 12);   // Red
    llFatal: SetConsoleTextAttribute(ConsoleHandle, 79);   // White on Red
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  case ALevel of
    llDebug: System.Write(#27'[90m');    // Gray
    llInfo:
      if ATagOnly then
        System.Write(#27'[34m')          // Blue
      else
        System.Write(#27'[0m');          // Default
    llWarning: System.Write(#27'[33m');  // Yellow
    llError: System.Write(#27'[31m');    // Red
    llFatal: System.Write(#27'[97;41m'); // White on Red
  end;
  {$ENDIF}
end;

function TSimpleLog.GetLogLevelStr(ALevel: TLogLevel): string;
begin
  case ALevel of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARNING';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
  else
    Result := 'UNKNOWN';
  end;
end;

procedure TSimpleLog.WriteColored(const AText: string; ALevel: TLogLevel;
  ATagOnly: Boolean);
{$IFDEF WINDOWS}
var
  ConsoleHandle: THandle;
  ConsoleInfo: TConsoleScreenBufferInfo;
  HaveOriginalAttributes: Boolean;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Console attributes apply immediately, so flush any text written before them.
  Flush(Output);
  ConsoleInfo := Default(TConsoleScreenBufferInfo);
  ConsoleHandle := THandle(TextRec(Output).Handle);
  HaveOriginalAttributes := GetConsoleScreenBufferInfo(ConsoleHandle, ConsoleInfo);
  {$ENDIF}

  SetConsoleColor(ALevel, ATagOnly);
  try
    Write(AText);
  finally
    {$IFDEF WINDOWS}
    Flush(Output);
    if HaveOriginalAttributes then
      SetConsoleTextAttribute(ConsoleHandle, ConsoleInfo.wAttributes);
    {$ENDIF}
    {$IFDEF UNIX}
    System.Write(#27'[0m');
    {$ENDIF}
  end;
end;

procedure TSimpleLog.WriteToConsole(const APrefix, ALevelTag, ASuffix: string;
  ALevel: TLogLevel);
begin
  if not ShouldUseColors then
    Write(APrefix, ALevelTag, ASuffix)
  else if FColorScope = ccsLine then
    WriteColored(APrefix + ALevelTag + ASuffix, ALevel, False)
  else
  begin
    Write(APrefix);
    WriteColored(ALevelTag, ALevel, True);
    Write(ASuffix);
  end;
  WriteLn;
end;

function TSimpleLog.GetBackupFileName: string;
begin
  Result := FLogFile + '.1';
end;

procedure TSimpleLog.CheckFileRotation;
var
  CurrentFileSize: Int64;
  FileStream: TFileStream;
  BackupFileName: string;
  TemporaryBackupFileName: string;
begin
  if not FileExists(FLogFile) then
    Exit;

  try
    FileStream := TFileStream.Create(FLogFile, fmOpenRead or fmShareDenyNone);
    try
      CurrentFileSize := FileStream.Size;
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      HandleError('Unable to inspect log file for rotation', E.Message);
      Exit;
    end;
  end;

  // If file size exceeds the limit, keep one bounded backup and start fresh.
  if CurrentFileSize >= FMaxFileSize then
  begin
    if FArchiveLogFiles then
    begin
      ArchiveActiveLog;
      Exit;
    end;

    BackupFileName := GetBackupFileName;

    if not FileExists(BackupFileName) then
    begin
      if not RenameFile(FLogFile, BackupFileName) then
        HandleError('Unable to rotate log file',
          'the active log could not be renamed; logging will continue in the active file');
      Exit;
    end;

    TemporaryBackupFileName := BackupFileName + '.tmp';
    if FileExists(TemporaryBackupFileName) and
      not SysUtils.DeleteFile(TemporaryBackupFileName) then
    begin
      HandleError('Unable to rotate log file',
        'the stale temporary backup could not be removed');
      Exit;
    end;

    if not RenameFile(BackupFileName, TemporaryBackupFileName) then
    begin
      HandleError('Unable to rotate log file',
        'the existing backup could not be staged');
      Exit;
    end;

    if not RenameFile(FLogFile, BackupFileName) then
    begin
      if not RenameFile(TemporaryBackupFileName, BackupFileName) then
        HandleError('Unable to restore the previous log backup',
          'the previous backup remains in the temporary backup file');
      HandleError('Unable to rotate log file',
        'the active log could not be renamed; logging will continue in the active file');
      Exit;
    end;

    if not SysUtils.DeleteFile(TemporaryBackupFileName) then
    begin
      HandleError('Unable to finish log rotation',
        'the previous temporary backup could not be removed');
    end;
  end;
end;

function TSimpleLog.GetArchiveFileName(AIndex: Integer): string;
begin
  Result := FLogFile + '.' + IntToStr(AIndex) + '.zip';
end;

function TSimpleLog.ZipActiveLog(const AZipFileName: string;
  out AErrorText: string): Boolean;
var
  LogZipper: TZipper;
begin
  Result := False;
  AErrorText := 'the archive was not created';
  try
    LogZipper := TZipper.Create;
    try
      LogZipper.FileName := AZipFileName;
      LogZipper.UseLanguageEncoding := True; // store UTF-8 entry names
      LogZipper.Entries.AddFileEntry(FLogFile, ExtractFileName(FLogFile));
      LogZipper.ZipAllFiles;
    finally
      LogZipper.Free;
    end;
    Result := FileExists(AZipFileName);
  except
    on E: Exception do
      AErrorText := E.Message;
  end;
end;

procedure TSimpleLog.ArchiveActiveLog;
var
  TemporaryArchiveFileName: string;
  ErrorText: string;
  i: Integer;
begin
  // Zip first, so the active log is only cleared once its archive exists.
  // Archives are numbered newest first: <log>.1.zip ... <log>.<MaxRotateFiles>.zip
  TemporaryArchiveFileName := FLogFile + '.zip.tmp';
  if FileExists(TemporaryArchiveFileName) and
    not SysUtils.DeleteFile(TemporaryArchiveFileName) then
  begin
    HandleError('Unable to archive log file',
      'the stale temporary archive could not be removed');
    Exit;
  end;

  if not ZipActiveLog(TemporaryArchiveFileName, ErrorText) then
  begin
    SysUtils.DeleteFile(TemporaryArchiveFileName);
    HandleError('Unable to archive log file',
      ErrorText + '; logging will continue in the active file');
    Exit;
  end;

  if FileExists(GetArchiveFileName(FMaxRotateFiles)) and
    not SysUtils.DeleteFile(GetArchiveFileName(FMaxRotateFiles)) then
  begin
    SysUtils.DeleteFile(TemporaryArchiveFileName);
    HandleError('Unable to archive log file',
      'the oldest archive could not be removed; logging will continue in the active file');
    Exit;
  end;

  for i := FMaxRotateFiles - 1 downto 1 do
    if FileExists(GetArchiveFileName(i)) and
      not RenameFile(GetArchiveFileName(i), GetArchiveFileName(i + 1)) then
    begin
      SysUtils.DeleteFile(TemporaryArchiveFileName);
      HandleError('Unable to archive log file',
        'an existing archive could not be renumbered; logging will continue in the active file');
      Exit;
    end;

  if not RenameFile(TemporaryArchiveFileName, GetArchiveFileName(1)) then
  begin
    SysUtils.DeleteFile(TemporaryArchiveFileName);
    HandleError('Unable to archive log file',
      'the new archive could not be moved into place; logging will continue in the active file');
    Exit;
  end;

  if not SysUtils.DeleteFile(FLogFile) then
    HandleError('Unable to finish log archiving',
      'the archived active log could not be removed; it will be archived again on the next rotation');
end;

procedure TSimpleLog.WriteToFile(const AMessage: string);
var
  FileStream: TFileStream;
  Dir: string;
begin
  if FLogFile = '' then
  begin
    HandleError('Unable to write log file', 'the log file name is empty');
    Exit;
  end;

  try
    // Create directory if it doesn't exist
    Dir := ExtractFilePath(FLogFile);
    if (Dir <> '') and not DirectoryExists(Dir) then
    begin
      if not EnsureDirectoryExists(Dir) then
      begin
        HandleError('Unable to create log directory', Dir);
        Exit;
      end;
    end;

    // Check if rotation is needed before writing
    CheckFileRotation;

    if FileExists(FLogFile) then
      FileStream := TFileStream.Create(FLogFile, fmOpenReadWrite or fmShareDenyNone)
    else
      FileStream := TFileStream.Create(FLogFile, fmCreate);

    try
      FileStream.Seek(0, soEnd); // Move to the end of the file
      
      // Write the message with newline
      if AMessage <> '' then
        FileStream.WriteBuffer(PChar(AMessage)^, Length(AMessage));
      if LineEnding <> '' then
        FileStream.WriteBuffer(PChar(LineEnding)^, Length(LineEnding));
    finally
      FileStream.Free;
    end;
  except
    on E: ESimpleLogError do
      raise;
    on E: Exception do
      HandleError('Unable to write log file', E.Message);
  end;
end;

procedure TSimpleLog.WriteLogEntry(ALevel: TLogLevel; const AMessage: string);
var
  Prefix: string;
  LevelTag: string;
  Suffix: string;
begin
  // Line shape: [yyyy-mm-dd hh:nn:ss.zzz] [LEVEL] message
  Prefix := '[' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + '] ';
  LevelTag := '[' + GetLogLevelStr(ALevel) + ']';
  Suffix := ' ' + AMessage;

  if odConsole in FOutputs then
    WriteToConsole(Prefix, LevelTag, Suffix, ALevel);

  if odFile in FOutputs then
    WriteToFile(Prefix + LevelTag + Suffix);
end;

procedure TSimpleLog.Log(ALevel: TLogLevel; const AMessage: string);
begin
  EnterLogLock;
  try
    if ShouldLog(ALevel) then
      WriteLogEntry(ALevel, AMessage);
  finally
    LeaveLogLock;
  end;
end;

procedure TSimpleLog.LogFmt(ALevel: TLogLevel; const AFormat: string; const AArgs: array of const);
begin
  EnterLogLock;
  try
    if ShouldLog(ALevel) then
      WriteLogEntry(ALevel, Format(AFormat, AArgs));
  finally
    LeaveLogLock;
  end;
end;

procedure TSimpleLog.Debug(const AMessage: string);
begin
  Log(llDebug, AMessage);
end;

procedure TSimpleLog.Info(const AMessage: string);
begin
  Log(llInfo, AMessage);
end;

procedure TSimpleLog.Warning(const AMessage: string);
begin
  Log(llWarning, AMessage);
end;

procedure TSimpleLog.Error(const AMessage: string);
begin
  Log(llError, AMessage);
end;

procedure TSimpleLog.Fatal(const AMessage: string);
begin
  Log(llFatal, AMessage);
end;

procedure TSimpleLog.Debug(const AFormat: string; const AArgs: array of const);
begin
  LogFmt(llDebug, AFormat, AArgs);
end;

procedure TSimpleLog.Info(const AFormat: string; const AArgs: array of const);
begin
  LogFmt(llInfo, AFormat, AArgs);
end;

procedure TSimpleLog.Warning(const AFormat: string; const AArgs: array of const);
begin
  LogFmt(llWarning, AFormat, AArgs);
end;

procedure TSimpleLog.Error(const AFormat: string; const AArgs: array of const);
begin
  LogFmt(llError, AFormat, AArgs);
end;

procedure TSimpleLog.Fatal(const AFormat: string; const AArgs: array of const);
begin
  LogFmt(llFatal, AFormat, AArgs);
end;

function TSimpleLog.EnsureDirectoryExists(const ADir: string): Boolean;
begin
  Result := ForceDirectories(ADir);
end;

procedure TSimpleLog.HandleError(const AOperation, ADetails: string);
begin
  if FErrorMode = lemRaise then
    raise ESimpleLogError.CreateFmt('%s "%s": %s',
      [AOperation, FLogFile, ADetails]);
end;

function TSimpleLog.GetOutputs: TOutputDestinations;
begin
  EnterLogLock;
  try
    Result := FOutputs;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetLogFile: string;
begin
  EnterLogLock;
  try
    Result := FLogFile;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetMinLevel: TLogLevel;
begin
  EnterLogLock;
  try
    Result := FMinLevel;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetMaxFileSize: Int64;
begin
  EnterLogLock;
  try
    Result := FMaxFileSize;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetSilent: Boolean;
begin
  EnterLogLock;
  try
    Result := FSilent;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetColorMode: TConsoleColorMode;
begin
  EnterLogLock;
  try
    Result := FColorMode;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetUseColors: Boolean;
begin
  EnterLogLock;
  try
    Result := FColorMode <> ccmNever;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetErrorMode: TLogErrorMode;
begin
  EnterLogLock;
  try
    Result := FErrorMode;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetColorScope: TConsoleColorScope;
begin
  EnterLogLock;
  try
    Result := FColorScope;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetArchiveLogFiles: Boolean;
begin
  EnterLogLock;
  try
    Result := FArchiveLogFiles;
  finally
    LeaveLogLock;
  end;
end;

function TSimpleLog.GetMaxRotateFiles: Integer;
begin
  EnterLogLock;
  try
    Result := FMaxRotateFiles;
  finally
    LeaveLogLock;
  end;
end;

initialization
  GLogLock := Default(TRTLCriticalSection);
  System.InitCriticalSection(GLogLock);

finalization
  System.DoneCriticalSection(GLogLock);

end.
