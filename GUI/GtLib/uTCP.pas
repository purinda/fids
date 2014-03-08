unit uTCP; // (C) Alphasoft Pty Ltd

interface

uses
  Forms, Classes, Windows, Messages, Dialogs, Contnrs, SysUtils,
  uGlobalDefs, uGT, uPacket,
  IdTCPClient, IdTCPServer, IdContext, IdTCPConnection, IdGlobal;

const
  LocalHost = '127.0.0.1';

  { $define Verbose }    // for ShowMessage(... errors
type
  aConnectionType = (ctNone, ctClient, ctServer);

  aConnection = record
    SecsAtStart: integer;
    ServerConnection: TIdTCPConnection;
    NetID: integer;
    // connection index - static during client/server connection
    Error: int;
    BytesRead: int;
  end;

  apConnection = ^aConnection;

  cTCP = class;

  cClientThread = class(TThread)
  private
    mTCP: cTCP;
  public
    constructor Create(tcp: cTCP); reintroduce;
    procedure Execute(); override;
  end;

  cTCP = class(Tobject)
    constructor Create(port: int; handler: aInputHandler; log: aLogProc = nil);
    // constructor  Create( port : int; wh : HWND; winMesgNo : int; log : aLogProc = nil );
    destructor Destroy; override;

  private
    mClientThread: cClientThread;
    mServer: TIdTCPServer;
    mClient: TIdTCPClient;

    mPort: integer;
    // Port numbers 1024 through 49151 are the registered ports used for IANA-registered services.
    mConnections: TList; // of aConnection
    mConnectionsLock: TMultiReadExclusiveWriteSynchronizer;

    // mhWindow : HWND;
    // mWinMesgNo : int;
    mThreadCount: int;
    mLog: procedure(ErrorNo: integer; const s: string) of object;
    mEr: int;
    mErMesg: string;
    mLastTx: int;
    mHost: string;

    mMaster: boolean;
    // procedure  ShutDownNow;
    procedure log(er: int; const s: string);
    procedure ProcessPkt(buf: TIdBytes; size, id: int);
    procedure ServerTick;
    procedure OnTCPConnect(AContext: TIdContext); // TCP threads
    procedure OnTCPExecute(AContext: TIdContext);
    procedure OnTCPDisconnect(AContext: TIdContext);
    procedure OnClientExecute(); // client thread only

  public
    mLocalIP: string; // own IP
    oInQ: cLockedQueue;
    mOnConnection: apConnectionEventReader;
    mShutdown: boolean;
    mConnected: boolean;
    procedure DB(n: int);
    function InitConnection(ip: string; con: aConnectionType): boolean;

    procedure Disconnect(shutdown: boolean);
    function ClientCount: int;
    procedure FreeMesg(const pm: apPacket);
    procedure Broadcast(const mesg: string; excludeID: int = -1;
      mType: aPktType = pkPassThrough); // to all pipes

    property Host: string read mHost;
    property Error: integer read mEr;
    property Master: boolean read mMaster;
  end;

implementation

uses
  uPoller {Seconds} ,
  IdStack, IdSocketHandle, IdBaseComponent, IdComponent, IdException,
  IdExceptionCore;

const
  MaxHandle = MaxInt;

constructor cTCP.Create(port: int; handler: aInputHandler; log: aLogProc = nil);

begin
  mPort := port;
  // mhWindow := wh;
  // mWinMesgNo := winMesgNo;
  mLog := log;

  mConnections := TList.Create; // of aConnection
  mConnectionsLock := TMultiReadExclusiveWriteSynchronizer.Create;
  // oInQ := cLockedQueue.Create( wh, winMesgNo );
  oInQ := cLockedQueue.Create(handler);
end;

destructor cTCP.Destroy;

var
  i: integer;
  pc: apConnection;
  b: byte;
{$HINTS OFF }
begin
  // ShutdownNow;
  if mClient <> nil then
  begin
    if mThreadCount > 0 then
      mClientThread.Terminate;
    while mThreadCount > 0 do
      Sleep(20);
    // Sleep( 10 );
    mClient.Disconnect; // and flush
    while mClient.Connected do
    begin
      b := mClient.IOHandler.ReadByte();
    end;
    FreeAndNil(mClient);
  end;

  if mConnections <> nil then
  begin
    for i := 0 to mConnections.Count - 1 do
    begin
      pc := mConnections[i];
      if pc <> nil then
      begin
        if pc.ServerConnection <> nil then
        begin
          pc.ServerConnection.Disconnect;
        end;
        Dispose(pc); // FreeMem ( pc );
      end;
    end;
    mConnections.Free;
  end;
  if mServer <> nil then
  begin
    // mServer.Active := false;
    FreeAndNil(mServer);
  end;
  FreeAndNil(oInQ);
  inherited Destroy;
end;
{$HINTS ON }

procedure cTCP.Disconnect(shutdown: boolean);

var
  s: string;
begin
  if shutdown then
    s := '<' + TagShutDown + '/>'
  else
    s := '<Disconnect/>';
  Broadcast(s, -1, pkDisconnect);
end;

{ procedure  cTCP.ShutdownNow;

  var
  i : integer;
  cp : apConnection;
  begin
  mShutdown := true;
  if mServer <> nil then  begin
  //mServer.Active := false;       $$$
  mConnectionsLock.BeginRead;                     // to avoid new connects hitting mConnections
  for i := 0 to mConnections.Count - 1 do  begin
  cp := mConnections[ i ];
  if cp <> nil then  begin
  try
  cp.ServerConnection.Disconnect;
  except
  end;
  end;
  end;
  mConnectionsLock.EndRead;                     // to avoid new connects hitting mConnections
  end;
  end; }

procedure cTCP.log(er: int; const s: string);

begin
  if er > mEr then
    mEr := er;
  if Assigned(mLog) then
    mLog(er, s);
end;

procedure cTCP.DB(n: int);

begin
  mEr := n;
end;

procedure cTCP.ServerTick;

begin
  if Seconds - mLastTx >= 5 then
    Broadcast('<Tick/>');
end;

function cTCP.InitConnection(ip: string; con: aConnectionType): boolean;

var
  i: integer;
  // con : boolean;
  Binding: TIdSocketHandle;
begin
  // mPort := port;
  // mServerIPsL := serverl;
  result := false;
  mShutdown := false;
  Poller.UnPollMe(ServerTick);
  if con = ctClient then
  begin

    if mServer <> nil then
      FreeAndNil(mServer); // mServer.Active := false;

    if mClient = nil then
      mClient := TIdTCPClient.Create(nil) // initialize GStack etc
    else
      mClient.Disconnect;
    mLocalIP := GStack.LocalAddresses[0];
    if ip <> '' then
    begin // if mServerIPsL[ x ] <> '' then  begin
      try
        begin
          // Log( stAttemptConnection, 'Attempt connection to server at ' + mServerIPsL[ x ] + ':' + IntToStr( mPort ) );
          mClient.port := mPort;
          mClient.Host := ip; // mServerIPsL[ x ];
          mClient.ConnectTimeout := 10000;
          mClient.ReadTimeout := 200;
          mClient.Connect;
        end;
      except
        log(stConnectionFailed, 'Connection to ' + ip + ':' + IntToStr(mPort) +
          ' failed');
      end;
      if mClient.Connected then
      begin // create client reader task
        log(stConnectionSucceded, 'Connection to ' + ip + ':' +
          IntToStr(mPort));
        mClientThread := cClientThread.Create(self);
        mClientThread.FreeOnTerminate := true;
        mConnected := true;
        mHost := ip;
        if Assigned(mOnConnection) then
          mOnConnection(ceConnected, ip);
        result := true;
        // break;
      end
      else
      begin
        mClient.Free;
        mClient := nil;
        result := false;
      end;
    end;
  end
  else if con = ctServer then
  begin
    Binding := nil;
    if Binding = nil then
    begin
      try
        begin // be server
          if mServer = nil then
            mServer := TIdTCPServer.Create(nil)
          else
            mServer.Active := false;
          // for i := 0 to GStack.LocalAddresses.Count - 1 do  begin
          // mLocalIP := GStack.LocalAddresses[ 0 ];
          // if mLocalIP <> LocalIP then  break;
          // end;

          mServer.OnConnect := OnTCPConnect;
          // procedure(AThread: TIdPeerThread) of object;
          mServer.OnExecute := OnTCPExecute;
          // procedure(AThread: TIdPeerThread) of object;
          mServer.OnDisconnect := OnTCPDisconnect;
          // procedure(AThread: TIdPeerThread) of object;
          // mServer.CommandHandlersEnabled := false;  // use onexecute connection
          mLocalIP := GStack.LocalAddresses[0];

          mServer.Bindings.Clear;
          for i := 0 to GStack.LocalAddresses.Count - 1 do
          begin
            Binding := mServer.Bindings.Add;
            Binding.ip := GStack.LocalAddresses[i];
            // eg 192.168.0.163    :1111
            Binding.port := mPort;
          end;
          Binding := mServer.Bindings.Add;
          Binding.ip := LocalHost;
          Binding.port := mPort;

          mServer.Active := true;
          result := mServer.Active;
          if result then
            Poller.PollMe(ServerTick);

        end
      except
{$IFNDEF DEBUG }
        log(stNoConToServer, 'Server ' + Binding.ip + ':' +
          IntToStr(Binding.port) + ' failed to start');
{$ENDIF }
        mServer.Free;
        mServer := nil;
      end;
    end;
    if result then
    begin
      // Global.HostID := Global.ID;       // todo move to sys mgr
{$IFNDEF DEBUG }
      log(stServerActive, 'SERVER active ' + GStack.LocalAddresses[0] + ':' +
        IntToStr(Binding.port));
{$ENDIF }
    end;
  end;
end;


// _______________________ ALL READER THREADS __________________________________

procedure cTCP.ProcessPkt(buf: TIdBytes; size, id: int);

var // posts or frees packets
  pm: apPacket;
begin
  pm := NewPkt(size);
  Move(buf[0], pm.pkType, Length(buf));
  // ugly transfer of bytes from indy to packet
  if pm.pkFlags = MagicFlags then
  begin
    pm.pkSrcID := id;
    if Length(buf) = size - SizeOf(size) then
    begin // all here
      if pm.pkType = pkPassThrough then
      begin
        oInQ.Post(pm, alTCP);
      end
      else if pm.pkType = pkDisconnect then
      begin
        oInQ.Post(pm, alTCP);
        if (mClient <> nil) and (mClientThread <> nil) then
          mClientThread.Terminate; // only known way to kill a client
      end
      else
      begin
        FreeMem(pm);
        mEr := 102; // unrecognised type
      end;
    end
    else
    begin
      FreeMem(pm);
      mEr := 103; // incomplete
    end;
  end
  else
  begin
    FreeMem(pm);
    mEr := 101; // incorrect magic flags
  end;
end;


// _______________________ INDI CREATED SERVER READER THREADS __________________

procedure cTCP.OnTCPConnect(AContext: TIdContext); // all TCP threads

var
  pc: apConnection;
  x: int;
begin
  log(stTCPevent, 'TCP Connection from ' +
    AContext.Connection.Socket.Binding.PeerIP);
  { AThread.Connection.WriteLn( '<FIDS.' + IntToStr( Global.ID ) + '/>' + BEL ); }
  if mServer <> nil then
  begin // I am host server
    if AContext.Connection <> nil then
    begin // create a new connection record
      New(pc);
      pc.SecsAtStart := Seconds;
      pc.BytesRead := 0;
      pc.ServerConnection := AContext.Connection;
      pc.NetID := -1;

      mConnectionsLock.BeginWrite;
      for x := 0 to mConnections.Count - 1 do
      begin
        if mConnections[x] = nil then
        begin // use an empty slot
          mConnections[x] := pc;
          pc.NetID := x;
          break;
        end;
      end;
      if pc.NetID < 0 then
      begin
        pc.NetID := mConnections.Add(pc); // or add it to the end
      end;
      mConnectionsLock.EndWrite;
    end;
  end;
end;

procedure cTCP.OnTCPExecute(AContext: TIdContext); // all Server threads

var
  s, b: int;
  pc: apConnection;
  buf: TIdBytes;
begin
  pc := nil;
  mConnectionsLock.BeginRead;
  for s := 0 to mConnections.Count - 1 do
  begin
    pc := mConnections[s];
    if (pc <> nil) and (pc.ServerConnection = AContext.Connection) then
      break;
  end; // identified this conection (thread)
  mConnectionsLock.EndRead;

  if not mShutdown and (pc <> nil) and mServer.Active then
  begin
    // s := pc.ServerConnection.IOHandler.ReadLongWord( false );
    for b := 0 to 3 do
      s := s + (pc.ServerConnection.IOHandler.ReadByte shl (8 * b));
    if (s >= SizeOfPktHdr) and mServer.Active then
    begin // at least 8 bytes
      SetLength(buf, s - SizeOf(s));
      pc.ServerConnection.IOHandler.ReadBytes(buf, s - SizeOf(s), false);
{$Q- overflow check }
      Inc(pc.BytesRead, s);
{$Q+ }
      ProcessPkt(buf, s, pc.NetID);
    end
  end;
end;

procedure cTCP.OnTCPDisconnect(AContext: TIdContext); // all TCP threads

var
  pc: apConnection;
  i: integer;
begin
  if mServer <> nil then
  begin
    mConnectionsLock.BeginWrite;
    for i := 0 to mConnections.Count - 1 do
    begin
      pc := mConnections[i];
      if pc <> nil then
        if pc.ServerConnection = AContext.Connection then
        begin // identified this conection
          mConnections[i] := nil;
          Dispose(pc);
          break;
        end;
    end;
    mConnectionsLock.EndWrite;
  end
  else if mClient <> nil then
    mClientThread.Terminate;
  log(stTCPevent, 'TCP Disconnect');
end;


// __________________________ CLIENT READ THREAD _______________________________

procedure cTCP.OnClientExecute(); // client thread only

var
  s, b: int;
  buf: TIdBytes;
begin
  if not mClientThread.Terminated and not mShutdown then
  begin
    try
      begin
        // s := mClient.IOHandler.ReadLongWord( false );  64 bits on 64 system
        s := 0; // 32 bit read
        for b := 0 to 3 do
          s := s + (mClient.IOHandler.ReadByte shl (8 * b));
        if not mClientThread.Terminated and (s >= SizeOfPktHdr) then
        begin // at least 8 bytes
          SetLength(buf, s - SizeOf(s));
          { try } mClient.IOHandler.ReadBytes(buf, s - SizeOf(s), false);
          { if s > 100 then
            DB( 0, 'DB' ); }
          { except   // ignore read timeout
            if not mClient.Connected then  begin
            mClientThread.Terminate;
            Log( stConnectionLost, 'Client connection lost' );
            end;
            end; }
          ProcessPkt(buf, s, 0);
        end;
      end;
    except
      on E: EIdException do
      begin
        if E is EIdReadTimeout then // ignore
        else
        begin
          mEr := stConnectionLost;
          mErMesg := E.Message;
          if not mClient.Connected then
            mClientThread.Terminate;
        end;
      end;
      else
      begin
        mEr := stConnectionLost; // todo prolly not
        // mErMesg := e.Message;
      end;
    end;
  end;
end;

procedure cClientThread.Execute(); // client thread only

begin
  InterlockedIncrement(mTCP.mThreadCount);
  try
    while not Terminated do
    begin // self destruct when terminated
      mTCP.OnClientExecute;
    end;
  finally
    InterlockedDecrement(mTCP.mThreadCount);
  end;
end;

// _________________________ MAIN THREAD _______________________________________

constructor cClientThread.Create(tcp: cTCP);

begin
  mTCP := tcp;
  inherited Create(false);
end;

function cTCP.ClientCount: int;

var
  p: int;
  cp: apConnection;
begin
  result := 0;
  if mServer <> nil then
  begin
    for p := 0 to mConnections.Count - 1 do
    begin
      if mConnections[p] <> nil then
      begin
        cp := mConnections[p];
        if cp <> nil then
          Inc(result);
      end;
    end
  end
  else if (mClient <> nil) and mConnected { mClient.Connected } then
    result := 1;
end;

procedure cTCP.FreeMesg(const pm: apPacket);

begin
  if pm <> nil then
  begin
    pm.FreePkt;
  end;
end;

procedure cTCP.Broadcast(const mesg: string; excludeID: int = -1;
  mType: aPktType = pkPassThrough);

var
  p: int;
  cp: apConnection;
  pm: apPacket;
  buf: TIdBytes; // aBytes;
begin
  if not mShutdown then
  begin
    mLastTx := Seconds;
    buf := PktBytes(mesg, mType);
    pm := @buf[0];
    if mServer <> nil then
    begin
      for p := 0 to mConnections.Count - 1 do
      begin
        if mConnections[p] <> nil then
        begin
          cp := mConnections[p];
          if (cp <> nil) and (p <> excludeID) then
          begin // and ( cp.BytesRead <> 0 )
            cp.ServerConnection.IOHandler.WriteDirect(TIdBytes(buf), pm.pkSize);
          end;
        end;
      end;
    end
    else if (mClient <> nil) and (0 <> excludeID) then
    begin
      mClient.IOHandler.WriteDirect(TIdBytes(buf), pm.pkSize);
    end;
  end;
end;

{
  end;	pm := @ buf[ 0 ];
  pm.pkType := mType;
}

end.
