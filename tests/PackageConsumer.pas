program PackageConsumer;

{$mode objfpc}{$H+}{$J-}

uses
  SimpleLog;

var
  Log: TSimpleLog;
begin
  Log := TSimpleLog.Console
    .SetColorMode(ccmNever)
    .SetErrorMode(lemRaise);

  if (Log.ColorMode <> ccmNever) or (Log.ErrorMode <> lemRaise) then
    Halt(1);
end.
