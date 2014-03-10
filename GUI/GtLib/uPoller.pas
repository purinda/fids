unit uPoller;

interface

uses
  Classes, Windows, Messages, SysUtils, Contnrs, SyncObjs,
{$IFDEF FMX }
  FMX.Types;
{$ELSE }
ExtCtrls;
{$ENDIF }

type
  aToPoll = procedure() of object;
  apToPoll = ^aToPoll;

  aOnTimeOutProc = reference to procedure;
  apOnTimeOutProc = ^aOnTimeOutProc;

  aTimeOut = record
    proc: aOnTimeOutProc;
    count: integer;
  end;

  apTimeOut = ^aTimeOut;

  aUIop = reference to procedure(const s1, s2: string; i: integer = 0);
  apUIop = ^aUIop;

  cPoller = class(TTimer)
    constructor Create; reintroduce;
    destructor Destroy; override;
  public
    procedure PollMe(cus: aToPoll);
    procedure PollMeTenths(cus: aToPoll);
    procedure UnPollMe(cus: aToPoll);
    procedure OnTimeOut(tenths: integer; proc: aOnTimeOutProc);
    procedure DoInMainThread(proc: aOnTimeOutProc;
      wait: boolean = false); overload;
    procedure DoInMainThread(proc: aUIop); overload;
  private
    mNextSecond: integer;
    mToPoll, mToPollTenths: TList; // of apCustomer
    mToTimeOut: TList; // of apTimeOut
    mToDoInMainThread: TQueue; // of aOnTimeOutProc
    mhWindow: HWND;
    mLock: TCriticalSection;

    mEntered: boolean;
    procedure PollAll;
    // procedure  PollList( list : TList );
    procedure TickHandler(Sender: TObject);
    procedure WMPoller(var wm: TMessage); // message WM_Hub;
  end;

var
  Poller: cPoller = nil;
  Seconds: integer = 0;
  tenths: integer = 0;

implementation

uses
{$IFDEF FMX }
  FMX.Dialogs;
{$ELSE }
  Dialogs;
{$ENDIF }

const
  WM_Poller = WM_USER + 846;

constructor cPoller.Create;

begin
  inherited Create(nil); // owner );
  mToPoll := TList.Create;
  mToPollTenths := TList.Create;
  mToTimeOut := TList.Create;
  mToDoInMainThread := TQueue.Create;
  mNextSecond := tenths + 10;
  mhWindow := AllocateHWnd(WMPoller); // dummy window so I can get messages
  mLock := TCriticalSection.Create;
  OnTimer := TickHandler;
  Interval := 100;
  Enabled := true;
end;

destructor cPoller.Destroy;

var
  cp: apToPoll;
  x: integer;
begin
  Enabled := false;
  for x := 0 to mToPoll.count - 1 do
  begin
    cp := mToPoll[x];
    FreeMem(cp);
  end;
  mToPoll.Free;
  mToTimeOut.Free;
  DeallocateHWnd(mhWindow);
  inherited;
end;

procedure PollList(list: TList);

var
  i: integer;
  p: apToPoll;
begin
  for i := 0 to list.count - 1 do
  begin
    p := list[i];
    try
      p^();
    except
    end;
  end;
end;

{$OVERFLOWCHECKS OFF}

// is default anyway
procedure cPoller.PollAll;

var
  i: integer;
  pto: apTimeOut;
begin
  if tenths >= mNextSecond then
  begin // 10 ticks per second
    mNextSecond := mNextSecond + 10;
    Inc(Seconds);
    PollList(mToPoll);
  end;
  PollList(mToPollTenths);

  i := 0;
  while i < mToTimeOut.count do
  begin
    pto := mToTimeOut[i];
    Dec(pto.count);
    if pto.count = 0 then
    begin // do and consume
      try
        pto.proc();
      except
      end;
      Dispose(pto);
      mToTimeOut.Delete(i);
    end
    else
      Inc(i);
  end;
end;

procedure cPoller.TickHandler(Sender: TObject); // every tenth of a second

begin
  Inc(tenths);
  if not mEntered then
  begin
    mEntered := true;
    PollAll;
    mEntered := false;
  end;
end;

procedure cPoller.PollMe(cus: aToPoll);

var
  cp: apToPoll;
begin
  cp := AllocMem(SizeOf(aToPoll));
  cp^ := cus; // copy proc and obj
  mToPoll.Add(cp);
end;

procedure cPoller.PollMeTenths(cus: aToPoll);

var
  cp: apToPoll;
begin
  cp := AllocMem(SizeOf(aToPoll));
  cp^ := cus; // copy proc and obj
  mToPollTenths.Add(cp);
end;

procedure DeList(list: TList; cus: aToPoll);

var
  cp: apToPoll;
  x: integer;
begin
  for x := 0 to list.count - 1 do
  begin
    cp := list[x]; // AllocMem( SizeOf( aToPoll ) );
    if (TMethod(cp^).Data = TMethod(cus).Data) and
      (TMethod(cp^).Code = TMethod(cus).Code) then
    begin
      FreeMem(cp);
      list.Delete(x); // could be problematic if entered
      break;
    end;
  end;
end;

procedure cPoller.UnPollMe(cus: aToPoll);

begin
  DeList(mToPoll, cus);
  DeList(mToPollTenths, cus);
end;

procedure cPoller.OnTimeOut(tenths: integer; proc: aOnTimeOutProc);

var
  pto: apTimeOut;
begin
  if (self <> nil) and (time > 0) and (time < 1000000) then
  begin
    New(pto);
    pto.count := tenths;
    pto.proc := proc;
    mToTimeOut.Add(pto);
  end;
end;

// _____________________________ VARIOUS THREADS _______________________________

procedure cPoller.DoInMainThread(proc: aOnTimeOutProc; wait: boolean = false);
// overload;

var
  p: apOnTimeOutProc;
  // e : TEvent;
begin
  if (self <> nil) and (mToDoInMainThread.count < 100) then
  begin
    New(p);
    p^ := proc;
    mLock.Acquire;
    mToDoInMainThread.Push(p);
    mLock.Release;
    if mhWindow <> 0 then
    begin
      if wait then
      begin
        SendMessage(mhWindow, WM_Poller, 0, 0);
        // signal main thread to empty the buffer
      end
      else
        PostMessage(mhWindow, WM_Poller, 0, 0);
      // signal main thread to empty the buffer
    end;
  end;
end;

procedure cPoller.DoInMainThread(proc: aUIop); // overload;

var
  p: apUIop;
  // e : TEvent;
begin
  if self <> nil then
  begin
    New(p);
    p^ := proc;
    mLock.Acquire;
    mToDoInMainThread.Push(p);
    mLock.Release;
    if mhWindow <> 0 then
    begin
      PostMessage(mhWindow, WM_Poller, 0, 0);
      // signal main thread to empty the buffer
    end;
  end;
end;

// _______________________________ main thread __________________________________

procedure cPoller.WMPoller(var wm: TMessage); // Main thread only here

var
  p: apOnTimeOutProc;
  e: TEvent;
begin
  if wm.Msg = WM_Poller then
  begin
    while mToDoInMainThread.count > 0 do
    begin
      mLock.Acquire;
      p := mToDoInMainThread.Pop;
      mLock.Release;
      if Enabled then
        p^();
      e := TEvent(wm.LParam);
      if (e <> nil) and (wm.WParam = 1) then
        e.SetEvent; // signal waiting thread to continue
      Dispose(p);
    end;
  end;
end;

initialization

if Poller = nil then
  Poller := cPoller.Create;

finalization

FreeAndNil(Poller);

{ USAGE

  Poller.OnTimeOut( 40,   // in 4 seconds do this stuff
  procedure() begin  //aOnTimeOutProc
  if not mHub.Connected then  begin
  if not mHub.Master then  begin
  FreeAndNil( mHub );
  ShowMessage( 'Could not find server ' + Server );
  end;
  end
  else NewConnection;
  end
  );
}
end.
