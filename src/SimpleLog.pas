unit SimpleLog;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, Termio{$ENDIF},
  Zipper;

type
  TThreadArchiveLogFile = class;

  ESimpleLogError = class(Exception);

  { Log level enumeration }
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
  
  { Output destination enumeration }
  TOutputDestination = (odConsole, odFile);
  TOutputDestinations = set of TOutputDestination;

  { Console color behavior }
  TConsoleColorMode = (ccmAuto, ccmAlways, ccmNever);

  { Logging I/O error behavior }
  TLogErrorMode = (lemSilent, lemRaise);
  
  { Simple logging advanced record }

  { TSimpleLog }

  TSimpleLog = record
  private
    FArchiveLogFiles : Boolean;
    FMaxRotateFiles : byte;
    FCurrentFileLog : byte;
    FNextFileLog : byte;
    FOutputs: TOutputDestinations;
    FLogFile: string;
    FMinLevel: TLogLevel;
    FMaxFileSize: Int64;
    FSilent: Boolean;
    FColorMode: TConsoleColorMode;
    FErrorMode: TLogErrorMode;
    class var ThreadArchiveLogFile : TThreadArchiveLogFile;

    class function CreateLogger(AOutputs: TOutputDestinations; const AFileName: string): TSimpleLog; static;
    procedure SetArchiveLogFiles(AValue : Boolean);
    procedure SetMaxRotateFiles(AValue : byte);
    function ShouldLog(ALevel: TLogLevel): Boolean;
    function ShouldUseColors: Boolean;
    procedure WriteToConsole(const AMessage: string; ALevel: TLogLevel);
    procedure WriteToFile(const AMessage: string);
    procedure SetConsoleColor(ALevel: TLogLevel);
    function GetLogLevelStr(ALevel: TLogLevel): string;
    function FormatMessage(ALevel: TLogLevel; const AMessage: string): string;
    procedure LogFmt(ALevel: TLogLevel; const AFormat: string; const AArgs: array of const);
    procedure WriteLogEntry(ALevel: TLogLevel; const AMessage: string);
    procedure CheckFileRotation;
    function GetBackupFileName: string;
    function EnsureDirectoryExists(const ADir: string): Boolean;
    procedure HandleError(const AOperation, ADetails: string);
    function GetOutputs: TOutputDestinations;
    function GetLogFile: string;
    function GetMinLevel: TLogLevel;
    function GetMaxFileSize: Int64;
    function GetSilent: Boolean;
    function GetColorMode: TConsoleColorMode;
    function GetUseColors: Boolean;
    function GetErrorMode: TLogErrorMode;

    //Archiving the logs file
    function ArchiveLogFile(const ALogFileName, AFileNameInArchive : String) : boolean;
    //Determines the prefix that is appended to the log file during its rotation.
    function DetermineSequenceNumberOfArchive() : byte;
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
    function SetErrorMode(AMode: TLogErrorMode): TSimpleLog;

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
    property MaxRotateFiles : byte read FMaxRotateFiles write SetMaxRotateFiles;
    property ArchiveLogFiles : Boolean read FArchiveLogFiles write SetArchiveLogFiles;
    property Silent: Boolean read GetSilent;
    property ColorMode: TConsoleColorMode read GetColorMode;
    property UseColors: Boolean read GetUseColors;
    property ErrorMode: TLogErrorMode read GetErrorMode;
  end;

  { TThreadArchiveLogFile }

  TThreadArchiveLogFile = class(TThread)  //Архивирует лог-файл и удаляет его
    private
      FLog : TSimpleLog;
      FLogFileName : string;
      FFileNameInArchive : string;
      { Private declarations }
    protected
      procedure Execute; override;
    public
      constructor Create(const ALogFileName, AFileNameInArchive : string; var ALog : TSimpleLog);
    end;

resourcestring
  lanExceededMaximumValue = 'The maximum allowed value of the log files has been exceeded';

implementation

const
  DEFAULT_MAX_FILE_SIZE = 10 * 1024 * 1024; // 10MB
  MIN_MAX_FILE_SIZE = 1024; // 1KB minimum to prevent I/O overload
  DEFAULT_ROTATE_FILES = 10;

var
  GLogLock: TRTLCriticalSection;

{ TThreadArchiveLogFile }

constructor TThreadArchiveLogFile.Create(
  const ALogFileName, AFileNameInArchive : string; var ALog : TSimpleLog);
begin
  inherited Create(False);
  FreeOnTerminate:= True;
  Priority := tpNormal;
  FLog := ALog;
  FLogFileName := ALogFileName;
  FFileNameInArchive := AFileNameInArchive;
end;

procedure TThreadArchiveLogFile.Execute;
begin
  try
    FLog.ArchiveLogFile(FLogFileName, FFileNameInArchive);
  finally
    Terminate
  end
end;

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
  Result.FMaxRotateFiles := DEFAULT_ROTATE_FILES;
  Result.ArchiveLogFiles := False;
  Result.FSilent := False;
  Result.FColorMode := ccmAuto;
  Result.FErrorMode := lemSilent;
end;

procedure TSimpleLog.SetArchiveLogFiles(AValue : Boolean);
begin
  if FArchiveLogFiles = AValue then Exit;
  FArchiveLogFiles := AValue
end;

procedure TSimpleLog.SetMaxRotateFiles(AValue : byte);
begin
  if FMaxRotateFiles = AValue then Exit;
  FMaxRotateFiles := AValue
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

procedure TSimpleLog.SetConsoleColor(ALevel: TLogLevel);
{$IFDEF WINDOWS}
var
  ConsoleHandle: THandle;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  ConsoleHandle := THandle(TextRec(Output).Handle);
  case ALevel of
    llDebug: SetConsoleTextAttribute(ConsoleHandle, 8);    // Gray
    llInfo: SetConsoleTextAttribute(ConsoleHandle, 7);     // White
    llWarning: SetConsoleTextAttribute(ConsoleHandle, 14); // Yellow
    llError: SetConsoleTextAttribute(ConsoleHandle, 12);   // Red
    llFatal: SetConsoleTextAttribute(ConsoleHandle, 79);   // White on Red
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
var
  ApplyColors: Boolean;
  {$IFDEF WINDOWS}
  ConsoleHandle: THandle;
  ConsoleInfo: TConsoleScreenBufferInfo;
  HaveOriginalAttributes: Boolean;
  {$ENDIF}
begin
  ApplyColors := ShouldUseColors;

  {$IFDEF WINDOWS}
  ConsoleInfo := Default(TConsoleScreenBufferInfo);
  ConsoleHandle := THandle(TextRec(Output).Handle);
  HaveOriginalAttributes := ApplyColors and
    GetConsoleScreenBufferInfo(ConsoleHandle, ConsoleInfo);
  {$ENDIF}

  if ApplyColors then
    SetConsoleColor(ALevel);

  try
    Write(AMessage);
  finally
    if ApplyColors then
    begin
      {$IFDEF WINDOWS}
      if HaveOriginalAttributes then
        SetConsoleTextAttribute(ConsoleHandle, ConsoleInfo.wAttributes);
      {$ENDIF}
      {$IFDEF UNIX}
      System.Write(#27'[0m');
      {$ENDIF}
    end;
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
    if ArchiveLogFiles then
      begin
        if MaxRotateFiles <> 0 then
          begin
            FNextFileLog := DetermineSequenceNumberOfArchive();
            BackupFileName := FLogFile + '.' + FNextFileLog.ToString
          end
        else
          begin
            BackupFileName := ChangeFileExt(FLogFile,
              FormatDateTime('_yyyymmdd_hhnnss', Now) + ExtractFileExt(FLogFile))
          end;

        // Simply rename the current file to create a backup
        if RenameFile(FLogFile, BackupFileName) then
          begin
            //We are archiving the file in a parallel stream. It can be archived in the main stream.
            ThreadArchiveLogFile := TThreadArchiveLogFile.Create(BackupFileName, BackupFileName, Self);
            sleep(50)
          end else
          begin
            // If rename fails, just delete the old file to start fresh
            SysUtils.DeleteFile(FLogFile)
          end
      end
    else
      begin
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
            'the previous temporary backup could not be removed')
        end
      end
  end
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
  FormattedMessage: string;
begin
  FormattedMessage := FormatMessage(ALevel, AMessage);

  if odConsole in FOutputs then
    WriteToConsole(FormattedMessage, ALevel);

  if odFile in FOutputs then
    WriteToFile(FormattedMessage);
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

function TSimpleLog.ArchiveLogFile(
  const ALogFileName, AFileNameInArchive : String) : boolean;
var
  OurZipper : TZipper;
  fa : Longint;
begin
  Result := False;
  OurZipper := TZipper.Create;
  try
    with OurZipper do
      begin
        FileName := ALogFileName + '.zip';
        // As the first argument, specify the name of the file that will be included in the zip archive.
        // The second argument is the file name as it appears in the zip archive, and later in the file system after unpacking.
        Entries.AddFileEntry(ALogFileName, AFileNameInArchive);
        // UTF8 encoding is written when UseLanguageEncoding is true.
        UseLanguageEncoding := true;  // Требуется FPC 3.2+
        // We perform the archiving operation and write the archived file.
        ZipAllFiles
      end;
    // If the file has been created and the creation time matches the current moment, then we set Result to True.
    fa := FileAge(OurZipper.FileName);
    if FileExists(OurZipper.FileName) then
      if DateTimeToStr(FileDateTodateTime(fa)) = DateTimeToStr(Now) then
        Result := True;
    // We delete the old file that has just been archived.
    if Result then
      begin
        if FileExists(ALogFileName) then
          try
            SysUtils.DeleteFile(ALogFileName)
          except on E : Exception do
            writeln('Error: ', E.Message)
          end
      end;
  finally
    OurZipper.Free
  end
end;

function TSimpleLog.DetermineSequenceNumberOfArchive : byte;
var
  i : byte;
  BiggestTime : Int64 = 0;
  ATimeFile : Int64 = 0;
  NoFile : byte = 0;
begin
  if FMaxRotateFiles >= FMaxRotateFiles.MaxValue then
    begin
      writeln(lanExceededMaximumValue);
      FMaxRotateFiles := FMaxRotateFiles.MaxValue
    end;

  for i := 1 to FMaxRotateFiles do
    begin
      if FileExists(FLogFile + '.' + i.ToString + '.zip') then
        begin
          ATimeFile := FileAge(FLogFile + '.' + i.ToString + '.zip');
          if BiggestTime < ATimeFile then
            begin
              FCurrentFileLog := i;
              if FCurrentFileLog = FMaxRotateFiles then
                FCurrentFileLog := 0;
              BiggestTime := ATimeFile
            end
        end
      else
        begin
          if i = 1 then
            begin
              FCurrentFileLog := 0;
              Inc(NoFile)
            end
          else
            begin
              Inc(NoFile);
              FCurrentFileLog := i - NoFile
            end
        end
    end;
  if NoFile <> i then
    begin
      Inc(FCurrentFileLog);
      Result := FCurrentFileLog
    end
  else
    begin
      FCurrentFileLog := 0;
      Inc(FCurrentFileLog);
      Result := FCurrentFileLog
    end
end;

initialization
  GLogLock := Default(TRTLCriticalSection);
  System.InitCriticalSection(GLogLock);

finalization
  System.DoneCriticalSection(GLogLock);

end.
