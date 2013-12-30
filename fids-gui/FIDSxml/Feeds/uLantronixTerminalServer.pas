unit uLantronixTerminalServer;

// (C) Digital Images

interface

uses
	SysUtils, uGT;

type
	aConnectionStatus = ( csNone, csConnected, csConnectionFailed, csKeyPadTimeout );
    aMode = ( mdNone, mdPollKeypad );
	apTSReader = procedure ( SrcIP : string; rx : TBytes ) of object;
    apNotify = procedure ( status : aConnectionStatus ) of object;

    iTerminalServer = interface
        procedure  Send( buf : string );
    	end;

function  TerminalServerInitialize( const ip : string;  port : int;
			Reader : apTSReader;  Notify : apNotify;  mode : aMode = mdNone ) : iTerminalServer; // cLantronixTerminalServer


implementation  // _____________________________________________________________

uses
	uThreadControl, ASCII, uPoller, uPacket, IdTCPConnection, IdTCPClient, IdExceptionCore, Windows;

type

	aSendPkt = record
    	buf : string;
        end;
    apSendPkt = ^ aSendPkt;

	cLantronixTerminalServer = class ( TInterfacedObject, iTerminalServer )
    	constructor Create( const ip : string;  port : int;  Reader : apTSReader;  Notify : apNotify;  mode : aMode = mdNone );
        destructor  Destroy;  override;
        private
            class var oWorkers : cWorkList;   // one list of workers for the whole terminal set
            mReader : apTSReader;
            mNotify : apNotify;
            mIP : string;
            mPort : int;
    		oTCPClient : TIdTCPClient;
            mStatus : aConnectionStatus;
    		mReaderThread : cCPUBackgroundThread;
    		mMode : aMode;
		    mConnected : Boolean;

			procedure  InitTCPClient;
			procedure  SetStatus( stat : aConnectionStatus );
        	procedure  Send( buf : string );
            procedure  PollKeypad( bt : cCPUBackgroundThread );
			procedure  PassThroughRx( const ip : string;  const buf : TBytes );
			procedure  Connected(Sender: TObject);
            procedure  ReadKeypad( bt : cCPUBackgroundThread );
    		end;


function  TerminalServerInitialize( const ip : string;  port : int;
			Reader : apTSReader;  Notify : apNotify;  mode : aMode = mdNone ) : iTerminalServer; // cLantronixTerminalServer

	var
    	lts : cLantronixTerminalServer;
	begin
    lts := cLantronixTerminalServer.Create( ip, port, Reader, Notify, mode );
    result := iTerminalServer( lts );
    end;


constructor cLantronixTerminalServer.  Create( const ip : string;  port : int;  Reader : apTSReader;
			Notify : apNotify;  mode : aMode = mdNone );

	begin
    mIP := ip;    mPort := port;   mReader := Reader;   mNotify := Notify;   mMode := mode;
    InitTCPClient;  // oTCPClient := TIdTCPClient.Create( nil );   // client conection to terminal server
    if oWorkers = nil then  oWorkers := cWorkList.Create;
    oWorkers.DoInBackground( ReadKeypad, 3 );       // start background tasks read and poll serial connection
    if mMode = mdPollKeypad then  mReaderThread := oWorkers.DoInBackground( PollKeypad, 3 );
    end;


destructor  cLantronixTerminalServer.  Destroy;  // override;  automatically called when interface disappears

	begin
    oWorkers.Free;  oWorkers := nil;
    try  if oTCPClient <> nil then  oTCPClient.Disconnect;
    except  end;  // swallow disconnect exception
    FreeAndNil( oTCPClient );
    end;


procedure  cLantronixTerminalServer.  InitTCPClient;

	begin
    FreeAndNil( oTCPClient );                // need a complete reset to do a re-connect win7
    oTCPClient := TIdTCPClient.Create( nil );
    oTCPClient.Host := mIP;
    oTCPClient.Port := mPort;
    oTCPClient.ReadTimeout := 2000;         // timeouts don't work on vista / win7  ( stuck with ~20 seconds )
    oTCPClient.ConnectTimeout := 20000;
    oTCPClient.OnConnected := Connected;    // only way to detect an actual connection
    end;


procedure  cLantronixTerminalServer.  Send( buf : string );

    var
    	pp : apSendPkt;
	begin
    if mMode = mdPollKeypad then  begin
        New( pp );
        pp.buf := buf;
        mReaderThread.Post( pp );
        end
    else if oTCPClient.Connected then  oTCPClient.IOHandler.Write( buf );
    end;

// _________________________ WORKER THREADS ____________________________________


procedure  cLantronixTerminalServer.  SetStatus( stat : aConnectionStatus );

	begin
    if mStatus <> stat then  begin
        if Assigned( mNotify ) then  begin
            Poller.DoInMainThread( procedure  begin  mNotify( stat );  end );
            end;
        mStatus := stat;
        if stat <> csConnected then  mConnected := false;
        end;
    end;


procedure  cLantronixTerminalServer.  PollKeypad( bt : cCPUBackgroundThread );

    var
    	buf : TBytes;
        pp : apSendPkt;
        ch : char;
	begin
    while not bt.Shutdown do  begin
    	Sleep( 1000 );  // every 1/2 second poll the keypad - matching a UDC operation
    	if mConnected then  begin
            if bt.Shutdown then  break;

            pp := bt.Read( 0 );         // read thread input queue without waiting
            if pp <> nil then  begin    // slip sent display string into poll string
            	if ( Length( pp.buf ) > 0 ) and ( pp.buf[ Length( pp.buf ) ] = ETX ) then  begin  // last char is etx => send command string
                    SetLength( buf, 2 );     // COM3 7304.1  01<1B>p<03>01HELLO out there 00000000<0D><03>01<1B>p<03>
                    buf[ 0 ] := Ord( '0' );   // address drop 1  todo handle set of drops ?
                    buf[ 1 ] := Ord( '1' );
            		for ch in pp.buf do  Add( buf, Ord( ch ) and $FF );
            		Dispose( pp );   pp := nil;
                    try  if oTCPClient.Connected then  oTCPClient.IOHandler.Write( buf );  //buf (48, 49, 27, 109, 48, 49, 65, 30, 3)
                    except  SetStatus( csConnectionFailed );  end;                         //     0   1  esc  m   0   1   A   RS  ETX
                    end;                                                           // buf (48, 49, 27, 104, 49, 49, 3)
                end;                                                               //      0   1   esc  h   1   1   etx  led 1 on

                                     // poll and handle display string
            SetLength( buf, 2 );     // COM3 7304.1  01<1B>p<03>01HELLO out there 00000000<0D><03>01<1B>p<03>
            buf[ 0 ] := Ord( '0' );   // address drop 1  todo handle set of drops
            buf[ 1 ] := Ord( '1' );
            // if pp = nil then  pp := bt.Read( 0 );         // read (if necessary ) thread input queue without waiting
            if pp <> nil then  begin    // slip sent display string into poll string
            	for ch in pp.buf do  Add( buf, Ord( ch ) and $FF );
                Add( buf, Ord( cr ) );   //buf (48, 49, 81, 97, 110, 116, 97, 115, 13, 27, 112, 3)
            	Dispose( pp );           //     0   1   Q   a    n    t    a   s   CR  esc  p   ETX
                end;
            Add( buf, Ord( esc ) );  // poll command
            Add( buf, Ord( 'p' ) );
            Add( buf, Ord( etx ) );  // buf[ 4 ] := Ord( etx );   // done
            try  if oTCPClient.Connected then  oTCPClient.IOHandler.Write( buf );  // buf	(48, 49, 27, 112, 3)
            except  SetStatus( csConnectionFailed );  end;
        	end;
        end;
    end;


procedure  cLantronixTerminalServer.  PassThroughRx( const ip : string;  const buf : TBytes );

	begin      // separte procedure means that anon method launch has unique input capture (lambda like)
    Poller.DoInMainThread(
        procedure  begin
            mReader( ip, buf );  // typically cKeyPad.HandleInput
            end
        );
    end;


procedure  cLantronixTerminalServer.  Connected(Sender: TObject);

	begin
    mConnected := true;
    SetStatus( csConnected );
    end;


procedure  cLantronixTerminalServer.  ReadKeypad( bt : cCPUBackgroundThread );

    var
    	raw, buf : TBytes;
        x : int;
	begin
    try  oTCPClient.Connect;   //  if not oTCPClient.Connected then
    except  SetStatus( csConnectionFailed );  end;
    Sleep( 2000 );
    while not bt.Shutdown do  begin
    	if mConnected and ( oTCPClient <> nil ) and ( oTCPClient.IOHandler <> nil ) then  begin
            SetLength( raw, 0 );
            try  begin
                oTCPClient.IOHandler.ReadBytes( raw, -1, false );
                if ( Length( raw ) > 0 ) and Assigned( mReader ) then  begin
                    SetStatus( csConnected );  // keypad responding
                    for x := 0 to Length( raw ) - 1 do  begin
                        Add( buf, raw[ x ] );    // build user buffers synchronised on etx bytes
                        if raw[ x ] = Ord( etx ) then  begin  // keypad mode end of block
                            if Length( buf ) > 3 then  begin   // ignore 0,1,etx poll echo
                                PassThroughRx( mIP, buf );
                                end;
                            SetLength( buf, 0 );
                            end;
                        end;
                    end;
                end;
            except
            	on E : EIdReadTimeout do  SetStatus( csKeyPadTimeout )
                else  SetStatus( csConnectionFailed );
                end;
            end
        else  begin
        	SetStatus( csConnectionFailed );
        	for x := 1 to 30 do  begin
                Sleep( 1000 );  // try again later
                if mConnected or bt.Shutdown then  break;
                end;   // 30 sec delay
            if not bt.Shutdown then  begin
        		try   begin
//                	if oTCPClient.IOHandler.CheckForDataOnSource( 1 );
//                    oTCPClient.IOHandler.ReadBytes( raw, -1, false );  // flush remaining data
//                    oTCPClient.Disconnect;
					InitTCPClient;
                    oTCPClient.Connect;  // retry
//					SetLength( raw, 1 );
//                    raw[ 0 ] := 0;  // send a null
//                    oTCPClient.IOHandler.Write( raw );
//                    SetStatus( csConnected );
                    end;
        		except  SetStatus( csConnectionFailed );  end;
                Sleep( 2000 );
                end;
            end;
    	end;
    end;

(*
192.168.0.211  30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 03 30 31 35 35 35 03 30 31 03 30 31 03 30 31 03 30
*)
end.
