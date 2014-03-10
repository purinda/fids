unit uGlobal;

interface

uses  Classes;

var
  ShutDown : boolean;
  Threads : TList;  // of active threads  TThread
  ThreadsRunning : integer;
  hLog : integer;   { debug log file handle if > 0 }

implementation

initialization
  hLog := -1;

end.
