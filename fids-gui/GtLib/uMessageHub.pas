unit uMessageHub;

//	handles external message passing and signalling using pipes and tcp


interface
// (C) ALphasoft Pty Ltd

uses
	uUDP, uTCP, uGT, uPacket, uGlobalDefs,  //  logging stuff, uPipe,
	Forms, Classes, Messages, Windows;
    {$DESCRIPTION '(C) Alphasoft'}

type
	asLink = set of aLink;
	aLinkID = packed record
		fLink : aLink;
		fID   : int;
		end;
	apLinkID = ^ aLinkID;

    aTCPMode = ( tcpNone, tcpCanClient, tcpClient, tcpMaster, tcpMutiMaster );
    //aPipeMode = ( pmNone, pmSlave, pmMaster );

	aReader = procedure( const mesg : string; link : apLinkID = nil ) of object;
	apReader = ^ aReader; // mirror reader

    aTCPState = ( tsNone, tsWaitIPSeek, tsWaitServerList, tsPassThrough );

	cMessageHub = class
			constructor	Create( log : aLogProc = nil );
            destructor  Destroy;   override;
		private
			oReaders : TList;  // of apReader ie local note recipients
			// mhWindow : HWND;
			mLinks : asLink;
			oTCP : cTCP;
            oUDPserver : cUDP;
			// oPipe : cPipe;
            mStartState : int;   // seconds
            mRetries : int;		// counter
			//mLocalIP : string;   // own IP
            mMyName : string;
            mPipeServerName, mTCPServerName : string;
            mServerIPs : TStringList;
    		mPort : int;
            mOnConnection : apConnectionEventReader;

			mLog : aLogProc;
            mEr : int;

			mMaster : boolean;
            mMode : aTCPMode;

            mTCPState : aTCPState;
    		mSMInit: Boolean;
            mDestroying : boolean;

            function    GetLocalIP : string;
			procedure   LogEr( er : int; const s : string );
			procedure   TryCreateUDP;
			procedure   SetNewState( st : aTCPState );
            procedure   SetOnConnection( handler : apConnectionEventReader );
			procedure   BeMaster;
			procedure   BroadcastToRemotes( const mesg : string; lid : apLinkID; mType : aPktType );
			procedure   StateMachine;   // comes here every second;
            function    GetConnected() : boolean;
			procedure	UdpHandler( q : cLockedQueue; var wm : TMessage );  // aInputHandler
			procedure	TcpHandler( q : cLockedQueue; var wm : TMessage );  // aInputHandler
            procedure	SetMaster( state : boolean );  // stand alone only - no comms
		public
			// procedure	WMMessageHub( var wm : TMessage ); // message WM_Hub;
			function	InitConnection( links : asLink;
							const MyName, PipeServerName, TCPServerName : string;
							mode : aTCPMode; serverl : TStringList; port : integer ) : boolean;
			procedure	Disconnect( shutdown : boolean = false );
			function	ClientCount : int;
			function	GetError : int;
			function	RegisterReader( const rdr : aReader ) : integer;
			procedure	DeRegisterReader( const rdr : aReader );
			function	Broadcast( const mesg : string; lid : apLinkID = nil; mType : aPktType = pkPassThrough ) : integer;
			property	Master : boolean  read mMaster  write SetMaster;
			property	Error : int  read GetError;
			property    LocalIP : string  read GetLocalIP;
            property	Connected : boolean  read  GetConnected;
            property	OnConnection : apConnectionEventReader  read mOnConnection  write SetOnConnection;
		end;

implementation

uses
	uPoller { calls statemachine every second }, SysUtils, Dialogs;

{const
	WM_TCP = WM_USER + $123;    // not used
	WM_UDP = WM_USER + $124;
	WM_Pipe = WM_USER + $125; }


constructor  cMessageHub.Create( log : aLogProc = nil );

	begin
	mLog := log;
    // mhWindow := form.Handle;
	oReaders := TList.Create;
	end;


destructor  cMessageHub.Destroy;

	var
		p : pointer;
	begin
    if not mDestroying then  begin
    	mDestroying := true;
        Disconnect;
      	Sleep( 100 );
        //if mTCP <> nil then  mTCP.mShutDown := true;
        //if mPipe <> nil then  mPipe.mShutDown := true;
        if Poller <> nil then  Poller.UnPollMe( StateMachine );
        FreeAndNil( oTCP );
        FreeAndNil( oUDPserver );
        // FreeAndNil( oPipe );
        // DeallocateHWnd( mhWindow );
        if oReaders <> nil then  for p in oReaders do  FreeMem( apReader( p ) );
        oReaders.Free;
        inherited;
    	end;
	end;


procedure  cMessageHub.LogEr( er : int; const s : string );

	begin
	 mEr := er;
	if Assigned( mLog ) then  mLog( er, s );
	end;


function   cMessageHub.ClientCount : int;

	begin
	result := 0;
	// if oPipe <> nil then  result := result + oPipe.ClientCount;
    if oTCP <> nil then  result := result + oTCP.ClientCount;
	end;


function    cMessageHub.GetLocalIP : string;

	begin
    result := '';
    if oTCP <> nil then   result := oTCP.mLocalIP;
    end;


function   cMessageHub.GetError : int;

	begin
	result := 0;
	// if oPipe <> nil then  result := oPipe.Error;
	end;


procedure   cMessageHub.SetOnConnection( handler : apConnectionEventReader );

	begin
    mOnConnection := handler;
    if oTCP <> nil then  begin
    	oTCP.mOnConnection := handler;
        if Connected then  handler( ceConnected, '' );
	    end;
    end;


procedure   cMessageHub.Disconnect( shutdown : boolean );

//	var
//		s : string;
	begin
    if oTCP <> nil then  oTCP.Disconnect( shutdown );

//	if oPipe <> nil then  begin
//		if shutdown then  s := MesgShutDown  else  s := '<Disconnect/>';
//    	oPipe.Broadcast( s, -1, pkDisconnect );
//	    end;
	end;


procedure	cMessageHub.UdpHandler( q : cLockedQueue; var wm : TMessage );  // aInputHandler

	var
		mp : apPacket;
		xml : string;
		lid : aLinkID;
	begin
	if ( wm.WParam = card( Ord( alUDP ) ) ) then  begin
		if ( oUDPserver <> nil ) and ( oUDPserver.oInQ.Count > 0 ) then  begin
			mp := oUDPserver.oInQ.Read();
			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
				xml := mp.ToString;
				lid.fLink := alNone;   lid.fID := 0;
				Broadcast( xml, @lid );
				end;
			oUDPserver.FreeMesg( mp );
			end;
		end;
    end;


procedure	cMessageHub.TcpHandler( q : cLockedQueue; var wm : TMessage );  // aInputHandler

	var
		mp : apPacket;
		xml : string;
		lid : aLinkID;
	begin
	if ( wm.WParam = card( Ord( alTCP ) ) ) and not oTCP.mShutdown then  begin
		if ( oTCP <> nil ) and ( oTCP.oInQ.Count > 0 ) then  begin
			mp := oTCP.oInQ.Read();
			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
				xml := mp.ToString;
				lid.fLink := alTCP;   lid.fID := mp.pkSrcID;
				Broadcast( xml, @lid );
				end;
			oTCP.FreeMesg( mp );
			end;
		end;
    wm.Result := 0;
    end;


procedure	cMessageHub.SetMaster( state : boolean );  // stand alone only - no comms

	begin
    if ( oTCP = nil ) then  mMaster := state;
    end;


//procedure	cMessageHub.WMMessageHub( var wm : TMessage );  // local message catcher
//
//	var
//		mp : apPacket;
//		xml : string;
//		lid : aLinkID;
//        handled : boolean;
//	begin
//    handled := true;
//	if ( wm.WParam = card( Ord( alPipe ) ) ) and not oPipe.mShutdown then  begin
//		if ( oPipe <> nil ) and ( oPipe.oInQ.Count > 0 ) then  begin
//			mp := oPipe.oInQ.Read();
//			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
//				xml := mp.ToString;
//				lid.fLink := alPipe;   lid.fID := mp.pkSrcID;
//				Broadcast( xml, @lid );
//				end;
//			oPipe.FreeMesg( mp );
//			end;
//		end
//	else if ( wm.WParam = card( Ord( alTCP ) ) ) and not oTCP.mShutdown then  begin
//		if ( oTCP <> nil ) and ( oTCP.oInQ.Count > 0 ) then  begin
//			mp := oTCP.oInQ.Read();
//			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
//				xml := mp.ToString;
//				lid.fLink := alTCP;   lid.fID := mp.pkSrcID;
//				Broadcast( xml, @lid );
//				end;
//			oTCP.FreeMesg( mp );
//			end;
//		end
//	else if ( wm.WParam = card( Ord( alUDP ) ) ) then  begin
//		if ( oUDPserver <> nil ) and ( oUDPserver.oInQ.Count > 0 ) then  begin
//			mp := oUDPserver.oInQ.Read();
//			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
//				xml := mp.ToString;
//				lid.fLink := alNone;   lid.fID := 0;
//				Broadcast( xml, @lid );
//				end;
//			oUDPserver.FreeMesg( mp );
//			end;
//		end
//    else  handled := false;
//
//    if handled then    wm.Result := 0
//    else    wm.Result := DefWindowProc( mhWindow, wm.Msg, wm.WParam, wm.LParam );
//	end;


{procedure	cMessageHub.WMMessageHub( var wm : TMessage );  // local message catcher

	var
		mp : apPacket;
		xml : string;
		lid : aLinkID;
	begin
	if ( wm.Msg = WM_Pipe ) and not mPipe.mShutdown then  begin
		if ( mPipe <> nil ) and ( mPipe.mInQ.Count > 0 ) then  begin
			mp := mPipe.mInQ.Read();
			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
				xml := mp.ToString;
				lid.fLink := alPipe;   lid.fID := mp.pkSrcID;
				Broadcast( xml, @lid );
				end;
			mPipe.FreeMesg( mp );
			end;
		end
	else if ( wm.Msg = WM_TCP ) and not mTCP.mShutdown then  begin
		if ( mTCP <> nil ) and ( mTCP.mInQ.Count > 0 ) then  begin
			mp := mTCP.mInQ.Read();
			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
				xml := mp.ToString;
				lid.fLink := alTCP;   lid.fID := mp.pkSrcID;
				Broadcast( xml, @lid );
				end;
			mTCP.FreeMesg( mp );
			end;
		end
	else if wm.Msg = WM_UDP then  begin
		if ( mUDPserver <> nil ) and ( mUDPserver.mInQ.Count > 0 ) then  begin
			mp := mUDPserver.mInQ.Read();
			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
				xml := mp.ToString;
				lid.fLink := alNone;   lid.fID := 0;
				Broadcast( xml, @lid );
				end;
			mUDPserver.FreeMesg( mp );
			end;
		end;
	end;}


procedure   cMessageHub.TryCreateUDP;

	begin
    if oUDPserver = nil then  begin
        try  oUDPserver := cUDP.Create( mPort, UdpHandler, mLog );      //     mhWindow, WM_Hub
        except    // eg port in use
            oUDPserver := nil;
            LogEr( erFailedToOpenUdpPort, 'UDP port ' + IntToStr( mPort ) + ' unavailable.' );
            //ShowMessage( 'UDP port ' + IntToStr( mPort ) + ' unavailable.' );
            end;
        end;
    end;



function   cMessageHub.InitConnection( links : asLink;
							const MyName, PipeServerName, TCPServerName : string;
							mode : aTCPMode; serverl : TStringList; port : integer ) : boolean;

    var
        useTCP : boolean;
	begin
	mLinks := links;
	mMaster := false;
    mMyName := MyName;
    mPipeServerName := PipeServerName;
    mTCPServerName := TCPServerName;
	// if mhWindow = 0 then  mhWindow := AllocateHWnd( WMMessageHub ); //  dummy window so I can get messages

    mServerIPs.Free;    mServerIPs := serverl;
    if mServerIPs = nil then  mServerIPs := TStringList.Create;
    if mServerIPs.IndexOf( LocalHost ) < 0 then   mServerIPs.Insert( 0, LocalHost );   // put local host at top of list

    mMode := mode;
    mPort := port;
	if alLocal in links then  begin
		if oReaders = nil then  oReaders := TList.Create;
		mMaster := true;
		end;

//	if alPipe in links then  begin  // preserve pipe on tcp reconfiguration
//		// mPipe.Free;
//        if oPipe = nil then  begin
//			oPipe := cPipe.CreatePipe( MyName, mPipeServerName, mhWindow, WM_Hub );
//        	{if mode = tcpCanClient then  Sleep( 300 )  // wait for a connection chatter
//            else}  //Sleep( 100 );
//            end
//        else  oPipe.Clear;
//        oPipe.Connect( true );
//        Sleep( 100 );
//		mMaster := oPipe.Master;
//        if not ( alTCP in links ) and oPipe.Connected and Assigned( mOnConnection ) then  mOnConnection();
//
//		end;

	if alTCP in links then  begin
        useTCP := mMode <> tcpNone;
//        if ( mMode = tcpCanClient ) and ( oPipe <> nil ) and ( oPipe.ClientCount > 0 )  then  begin
//            useTCP := false;  // already pipe connected so don't bother
//	        end;

        if useTCP then  begin    // todo
            mMaster := false;    // may set back to true if cannot client in
            TryCreateUDP;
            if oTCP = nil then  oTCP := cTCP.Create( port, TcpHandler, mLog );  //  mhWindow, WM_Hub
            oTCP.mOnConnection := mOnConnection;
            mTCPState := tsNone;
            if not mSMInit then  Poller.PollMe( StateMachine );
            StateMachine;
            mSMInit := true;
            end;
        end;
	result := false;
	end;


function    cMessageHub.GetConnected() : boolean;

	begin
    result := false;
//    if oPipe <> nil then  begin
//    	if oPipe.Connected then  result := true;
//	    end;
    if oTCP <> nil then  begin
        if oTCP.ClientCount > 0 then  result := true;
    	end;
    end;


procedure  cMessageHub.SetNewState( st : aTCPState );

	begin
    mTCPState := st;
    mStartState := Seconds;
    mRetries := 0;
//    if st = tsPassThrough then  begin
//        if Assigned( mOnConnection ) then  mOnConnection( ceConnected, oTCP.Host );
//    	end;
    end;


procedure  cMessageHub.BeMaster;

	begin
    oTCP.InitConnection( '', ctServer );
    TryCreateUDP;
    if oUDPserver <> nil then  begin
        oUDPserver.IamServerName := mMyName;
        end;
    mMaster := true;
    SetNewState( tsPassThrough );
    if Assigned( mOnConnection ) then  mOnConnection( ceConnected, '' );
    end;


procedure  cMessageHub.StateMachine;   // comes here every second - see poller;

    var
    	x : int;
        done, con : boolean;

	begin
    if mMode <> tcpNone then  begin
        repeat
            done := true;
            case mTCPState of
                tsNone :  begin
                    mMaster := false;
                    if mMode = tcpMaster then  BeMaster
                    else   SetNewState( tsWaitIPSeek );
                    done := false;
                    end;

                tsWaitIPSeek : begin
                    TryCreateUDP;
                    if ( oUDPserver = nil ) and ( mMode <= tcpClient ) then  begin
                        if oTCP.InitConnection( LocalHost, ctClient ) then  begin
                            SetNewState( tsPassThrough );
                            // con := true;
                            break;
                            end;

                    	end;
                    if ( oUDPserver <> nil ) and ( oUDPserver.ServerName <> '' ) and ( oUDPserver.ServerName = mTCPServerName ) then  begin
                        if oTCP.InitConnection( oUDPserver.ServerIP, ctClient ) then  SetNewState( tsPassThrough )
                        else  SetNewState( tsWaitServerList );
                        FreeAndNil( oUDPserver );  // not master so don't hog this port
                        end
                    else if mRetries < 2 then  begin
                        Inc( mRetries );
                        if oUDPserver <> nil then  oUDPserver.RequestServer( mTCPServerName );
                        end
                    else  begin
                        SetNewState( tsWaitServerList );
                        done := false;
                        end;
                    end;

                tsWaitServerList : begin
                    con := false;
                    if mServerIPs <> nil then  begin
                        for x := 0 to mServerIPs.Count - 1 do  begin
                            if oTCP.InitConnection( mServerIPs[ x ], ctClient ) then  begin
                                SetNewState( tsPassThrough );
                                con := true;
                                break;
                                end;
                            end;
                        end;

                    if ( not con ) and ( mMode > tcpClient ) then  begin   // can master or
                        BeMaster;
                        end
                    else  if not con then  SetNewState( tsWaitIPSeek );
                    end;

                tsPassThrough : begin
                    if Seconds - mStartState > oTCP.ClientCount * 5 then  begin   // periodic server announcement
                        mStartState := Seconds;
                        if mMaster then  begin
                        	TryCreateUDP;
                            if oUDPserver <> nil then  begin
                            	if ( oUDPserver.ServerIP <> '' ) and
                                    ( oUDPserver.ServerIP <> oTCP.mLocalIP ) then  begin
                                    mTCPState := tsNone;  // restart if someone else serving as well !
                                    LogEr( erForcedTCPReconfiguration, 'Forced TCP Reconfiguration' );
                                    end
                                else  oUDPserver.AnnounceServer;
	                            end;
                            end;
                        end;
                    end;
                end;
        	until done;
        end;
    end;


function  cMessageHub.RegisterReader( const rdr : aReader ) : integer;

	// add a new reader to the broadcast list
	var
		rp : apReader;
	begin
	rp := AllocMem( SizeOf( aReader ) );  // rp := New( apReader^^ ); compiler cannot cope
	rp^ := rdr;   // copy proc and obj
	Result := oReaders.Add( rp );
	end;


procedure cMessageHub.DeRegisterReader( const rdr : aReader );

	var
		x : integer;
		rp : apReader;
	begin
	if oReaders <> nil then  begin
		for x := 0 to oReaders.Count - 1 do  begin
			rp := oReaders[ x ];
			// if rp = @rdr then  begin     NFG
            if ( TMethod(rp^).Code = TMethod(rdr).Code ) and ( TMethod(rp^).Data = TMethod(rdr).Data )  then  begin
				oReaders.Delete( x );
				FreeMem( rp );
				break;
				end;
			end;
		end;
	end;


procedure  cMessageHub.BroadcastToRemotes( const mesg : string; lid : apLinkID; mType : aPktType );

	var
		e : integer;
	begin
//	if ( oPipe <> nil ) then  begin
//		if ( lid = nil ) or ( lid.fLink <> alPipe ) then  e := -1  else  e := lid.fID;
//		oPipe.Broadcast( mesg, e );        // to all connected pipes except source
//		end;
	if oTCP <> nil then  begin
		if ( lid = nil ) or ( lid.fLink <> alTCP ) then  e := -1  else  e := lid.fID;
		oTCP.Broadcast( mesg, e );         // to all connected pipes except source
		end;
	//	NetCast( nxml, netID );            // Server -> all connected tcp clients   or client to server
	end;


function   cMessageHub.Broadcast( const mesg : string; lid : apLinkID = nil; mType : aPktType = pkPassThrough ) : integer;

	var
		i, e : integer;
		p : apReader;
	begin
    result := 0;
	if oReaders <> nil then begin
    i := oReaders.Count - 1;	//i := 0;
    	result := i + 1;
		if ( lid = nil ) or ( lid.fLink <> alLocal ) then  e := -1  else  e := lid.fID;
		while ( i >= 0 ) and ( i < oReaders.Count ) do  begin  // while i < mReaders.Count do  begin
			try   begin
				if e <> i then  begin
					p := oReaders[ i ];
					if p <> nil then  p^( mesg, lid );
					end;
				end;
			finally  Dec( i );   end;  //  Inc( i );
			end;
		end;
    BroadcastToRemotes( mesg, lid, mType );
	end;


{ ______________________________ USEAGE ________________________________________

TestServers( ips );
SetServer
OnMesgRecv := Recv( mesg : string )  // to preparse to TObj
RegisterReader( proc of obj )
Broadcast( mesg : string, ( + subset ) );
Disconnect
Restart



10K message and echo response times
mesg size B		TCP			TCP:localhost	PIPE
500				370		    84              57       uSec
 42             214     	83              56       uSec




procedure  cConnection.Connect;

    var
        timeout : boolean;
	begin
    oHub.OnConnection := NewConnection;
    timeout := false;
    // oHub.RegisterReader( MessageReader );
    case ConnectionType of
        ctLocalClient	: begin
			oHub.InitConnection( [ alLocal, alPipe ], MyName, ServerName, ServerName,
            tcpNone, TStringList( nil ), PortNo );
            timeout := true;
            end;
        ctLocalServer	: begin
			oHub.InitConnection( [ alLocal, alPipe ], ServerName, ServerName, ServerName,
            tcpNone, TStringList( nil ), PortNo );
            end;
        ctRemoteClient	: begin
			oHub.InitConnection( [ alPipe, alTCP ], MyName, ServerName, ServerName,
            tcpClient, TStringList( nil ), PortNo );
            timeout := true;
            end;
        ctServer	: begin
			oHub.InitConnection( [ alLocal, alPipe, alTCP ], MyName, ServerName, ServerName,
            tcpMaster, TStringList( nil ), PortNo );
            end;
        ctMultiServer	: begin
			oHub.InitConnection( [ alLocal, alPipe, alTCP ], MyName, ServerName, ServerName,
            tcpMutiMaster, TStringList( nil ), PortNo );
            end;
    	end;
    if timeout then  begin
        Poller.OnTimeOut( 100,              // in 10 seconds time check the connection
            procedure() begin  //aOnTimeOutProc
                if not oHub.Connected then  begin
                    if not oHub.Master then  begin
                        FreeAndNil( oHub );
                        ShowMessage( 'Could not find server ' + ServerName );
                        end;
                    end
                else NewConnection;        // should be redundant
                end
            );
    	end;
    end;

 }
end.
