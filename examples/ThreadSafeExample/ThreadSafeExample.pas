program ThreadSafeExample;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes, SysUtils, SimpleLog;

type
  TLoggerThread = class(TThread)
  private
    FLogger: TSimpleLog;
    FThreadNum: Integer;
  public
    constructor Create(ALogger: TSimpleLog; AThreadNum: Integer);
    procedure Execute; override;
  end;

constructor TLoggerThread.Create(ALogger: TSimpleLog; AThreadNum: Integer);
begin
  FLogger := ALogger;
  FThreadNum := AThreadNum;
  inherited Create(False);
end;

procedure TLoggerThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    FLogger.Info('Thread %d: Message %d', [FThreadNum, i]);
    Sleep(10); // Small delay to create interleaving
  end;
end;

var
  Log: TSimpleLog;
  Threads: array[1..3] of TLoggerThread;
  i: Integer;
begin
  WriteLn('=== Concurrent Logging & Silent Mode Example ===');
  WriteLn('Note: Multiple threads logging concurrently with basic thread safety.');
  WriteLn('SimpleLog handles concurrent access reasonably well for most use cases.');
  WriteLn;
  
  // Initialize logger for both console and file
  Log := TSimpleLog.Both('threaded.log')
    .SetMinLevel(llInfo);
  
  WriteLn('Starting concurrent logging...');
  
  // Create and start multiple threads
  for i := 1 to 3 do
  begin
    Threads[i] := TLoggerThread.Create(Log, i);
  end;
  
  // Wait for all threads to complete
  for i := 1 to 3 do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;
  
  WriteLn('All threads completed. Flushing...');
  Log.Flush;
  
  WriteLn;
  WriteLn('=== Testing Silent Mode ===');
  
  Log.Info('This message will appear');
  
  // Enable silent mode
  Log.SetSilent(True);
  Log.Info('This message will NOT appear (silent mode)');
  Log.Error('This error will NOT appear (silent mode)');
  
  // Disable silent mode
  Log.SetSilent(False);
  Log.Info('Silent mode disabled - this message appears again');
  
  WriteLn('Example completed. Check threaded.log for file output.');
  WriteLn('Note: Output may show interleaved messages due to concurrent access.');
end.
