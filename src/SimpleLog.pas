unit SimpleLog;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows {$ENDIF};

type
  { Log level enumeration }
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
  
  { Output destination enumeration }
  TOutputDestination = (odConsole, odFile);
  TOutputDestinations = set of TOutputDestination;
  
  { Simple logging advanced record }
  TSimpleLog = record
  private
    FOutputs: TOutputDestinations;
    FLogFile: string;
    FMinLevel: TLogLevel;
    FMaxFileSize: Int64;
    FSilent: Boolean;

    function ShouldLog(ALevel: TLogLevel): Boolean;
    procedure WriteToConsole(const AMessage: string; ALevel: TLogLevel);
    procedure WriteToFile(const AMessage: string);
    procedure SetConsoleColor(ALevel: TLogLevel);
    procedure ResetConsoleColor;
    function GetLogLevelStr(ALevel: TLogLevel): string;
    function FormatMessage(ALevel: TLogLevel; const AMessage: string): string;
    procedure CheckFileRotation;
    function GetRotationFileName: string;
    function EnsureDirectoryExists(const ADir: string): Boolean;
  public
    { Factory methods }
    class function Console: TSimpleLog; static;
    class function FileLog(const AFileName: string): TSimpleLog; static;
    class function Both(const AFileName: string): TSimpleLog; static;

    procedure Finalize;

    { Configuration }
    function SetOutputs(AOutputs: TOutputDestinations): TSimpleLog;
    function SetFile(const AFileName: string): TSimpleLog;
    function SetMinLevel(ALevel: TLogLevel): TSimpleLog;
    function SetMaxFileSize(ASize: Int64): TSimpleLog;
    function SetSilent(ASilent: Boolean): TSimpleLog;

    { Logging methods }
    procedure Log(ALevel: TLogLevel; const AMessage: string);
    procedure LogFmt(ALevel: TLogLevel; const AFormat: string; const AArgs: array of const);
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
    property Outputs: TOutputDestinations read FOutputs write FOutputs;
    property LogFile: string read FLogFile write FLogFile;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    property MaxFileSize: Int64 read FMaxFileSize write FMaxFileSize;
    property Silent: Boolean read FSilent write FSilent;
  end;


implementation

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
class function TSimpleLog.Console: TSimpleLog; static;
begin
  Result.FOutputs := [odConsole];
  Result.FLogFile := '';
  Result.FMinLevel := llDebug;
  Result.FMaxFileSize := DEFAULT_MAX_FILE_SIZE;
  Result.FSilent := False;
end;

class function TSimpleLog.FileLog(const AFileName: string): TSimpleLog;
begin
  Result.FOutputs := [odFile];
  Result.FLogFile := AFileName;
  Result.FMinLevel := llDebug;
  Result.FMaxFileSize := DEFAULT_MAX_FILE_SIZE;
  Result.FSilent := False;
end;

class function TSimpleLog.Both(const AFileName: string): TSimpleLog;
begin
  Result.FOutputs := [odConsole, odFile];
  Result.FLogFile := AFileName;
  Result.FMinLevel := llDebug;
  Result.FMaxFileSize := DEFAULT_MAX_FILE_SIZE;
  Result.FSilent := False;
end;

procedure TSimpleLog.Finalize;
begin
  // Kept for source compatibility with v0.5.1; no per-instance cleanup is needed.
end;

function TSimpleLog.SetOutputs(AOutputs: TOutputDestinations): TSimpleLog;
begin
  FOutputs := AOutputs;
  Result := Self;
end;

function TSimpleLog.SetFile(const AFileName: string): TSimpleLog;
begin
  FLogFile := AFileName;
  Result := Self;
end;

function TSimpleLog.SetMinLevel(ALevel: TLogLevel): TSimpleLog;
begin
  FMinLevel := ALevel;
  Result := Self;
end;

function TSimpleLog.SetMaxFileSize(ASize: Int64): TSimpleLog;
begin
  // Enforce minimum file size to prevent I/O overload
  if ASize < MIN_MAX_FILE_SIZE then
    FMaxFileSize := MIN_MAX_FILE_SIZE
  else
    FMaxFileSize := ASize;
  Result := Self;
end;

function TSimpleLog.SetSilent(ASilent: Boolean): TSimpleLog;
begin
  FSilent := ASilent;
  Result := Self;
end;

function TSimpleLog.ShouldLog(ALevel: TLogLevel): Boolean;
begin
  Result := (not FSilent) and (ALevel >= FMinLevel);
end;

procedure TSimpleLog.SetConsoleColor(ALevel: TLogLevel);
begin
  {$IFDEF WINDOWS}
  case ALevel of
    llDebug: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 8);    // Gray
    llInfo: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);     // White
    llWarning: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14); // Yellow
    llError: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 12);   // Red
    llFatal: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 79);   // White on Red
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  case ALevel of
    llDebug: System.Write(#27'[90m');    // Gray
    llInfo: System.Write(#27'[0m');      // Default
    llWarning: System.Write(#27'[33m');  // Yellow
    llError: System.Write(#27'[31m');    // Red
    llFatal: System.Write(#27'[97;41m'); // White on Red
  end;
  {$ENDIF}
end;

procedure TSimpleLog.ResetConsoleColor;
begin
  {$IFDEF WINDOWS}
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7); // Default
  {$ENDIF}
  {$IFDEF UNIX}
  System.Write(#27'[0m');
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

function TSimpleLog.FormatMessage(ALevel: TLogLevel; const AMessage: string): string;
begin
  Result := Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
     GetLogLevelStr(ALevel),
     AMessage]);
end;

procedure TSimpleLog.WriteToConsole(const AMessage: string; ALevel: TLogLevel);
begin
  SetConsoleColor(ALevel);
  WriteLn(AMessage);
  ResetConsoleColor;
end;

function TSimpleLog.GetRotationFileName: string;
var
  Counter: Integer;
  Extension: string;
  Stamp: string;
begin
  Extension := ExtractFileExt(FLogFile);
  Stamp := FormatDateTime('_yyyymmdd_hhnnss_zzz', Now);
  Result := ChangeFileExt(FLogFile, Stamp + Extension);
  Counter := 1;

  while FileExists(Result) do
  begin
    Result := ChangeFileExt(FLogFile, Format('%s_%d%s', [Stamp, Counter, Extension]));
    Inc(Counter);
  end;
end;

procedure TSimpleLog.CheckFileRotation;
var
  CurrentFileSize: Int64;
  FileStream: TFileStream;
  BackupFileName: string;
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
    // If we can't access the file, exit silently
    Exit;
  end;

  // If file size exceeds the limit, create a backup and start fresh
  if CurrentFileSize >= FMaxFileSize then
  begin
    BackupFileName := GetRotationFileName;
    
    try
      // Simply rename the current file to create a backup
      if RenameFile(FLogFile, BackupFileName) then
      begin
        // File was successfully renamed, new writes will create a fresh file
      end else
      begin
        // If rename fails, just delete the old file to start fresh
        SysUtils.DeleteFile(FLogFile);
      end;
    except
      // If both rename and delete fail, try to truncate
      try
        FileStream := TFileStream.Create(FLogFile, fmCreate);
        FileStream.Free;
      except
        // Last resort - ignore the error and continue
      end;
    end;
  end;
end;

procedure TSimpleLog.WriteToFile(const AMessage: string);
var
  FileStream: TFileStream;
  Dir: string;
begin
  if FLogFile = '' then
    Exit;

  // Create directory if it doesn't exist
  Dir := ExtractFilePath(FLogFile);
  if (Dir <> '') and not DirectoryExists(Dir) then
  begin
    if not EnsureDirectoryExists(Dir) then
      Exit; // If we can't create the directory, exit silently
  end;

  // Check if rotation is needed before writing
  CheckFileRotation;

  try
    if FileExists(FLogFile) then
      FileStream := TFileStream.Create(FLogFile, fmOpenReadWrite or fmShareDenyNone)
    else
      FileStream := TFileStream.Create(FLogFile, fmCreate);

    try
      FileStream.Seek(0, soEnd); // Move to the end of the file
      
      // Write the message with newline
      FileStream.Write(PChar(AMessage)^, Length(AMessage));
      FileStream.Write(PChar(LineEnding)^, Length(LineEnding));
    finally
      FileStream.Free;
    end;
  except
    // Silent failure - don't crash the application for logging errors
  end;
end;

procedure TSimpleLog.Log(ALevel: TLogLevel; const AMessage: string);
var
  FormattedMessage: string;
begin
  if not ShouldLog(ALevel) then
    Exit;

  EnterLogLock;
  try
    if not ShouldLog(ALevel) then
      Exit;

    FormattedMessage := FormatMessage(ALevel, AMessage);

    // Output to console if enabled
    if odConsole in FOutputs then
      WriteToConsole(FormattedMessage, ALevel);

    // Output to file if enabled
    if odFile in FOutputs then
      WriteToFile(FormattedMessage);
  finally
    LeaveLogLock;
  end;
end;

procedure TSimpleLog.LogFmt(ALevel: TLogLevel; const AFormat: string; const AArgs: array of const);
begin
  if not ShouldLog(ALevel) then
    Exit;

  Log(ALevel, Format(AFormat, AArgs));
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

initialization
  System.InitCriticalSection(GLogLock);

finalization
  System.DoneCriticalSection(GLogLock);

end.
