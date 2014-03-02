unit uMessageHub;

//	handles external message passing and signalling using pipes and tcp


interface
// (C) ALphasoft Pty Ltd

uses
	uPipe, uUDP, uTCP, uGT, uPacket, uGlobalDefs { logging stuff },
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
    aPipeMode = ( pmNone, pmSlave, pmMaster );

	aReader = procedure( const mesg : string; link : apLinkID = nil ) of object;
	apReader = ^ aReader;

    aTCPState = ( tsNone, tsWaitIPSeek, tsWaitServerList, tsPassThrough );

	cMessageHub = class
			constructor	Create( log : aLogProc = nil );
            destructor  Destroy;   override;
		private
			mReaders : TList;  // of apReader ie local note recipients
			mhWindow : HWND;
			mLinks : asLink;
			mTCP : cTCP;
            mUDPserver : cUDP;
			mPipe : cPipe;
            mStartState : int;   // seconds
            mRetries : int;		// counter
			//mLocalIP : string;   // own IP
            mMyName : string;
            mPipeServerName, mTCPServerName : string;
            mServerIPs : TStringList;
    		mPort : int;
            mOnConnection : aEventHandler;

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
            procedure   SetOnConnection( handler : aEventHandler );
			procedure   BeMaster;
			procedure   BroadcastToRemotes( const mesg : string; lid : apLinkID; mType : aPktType );
			procedure   StateMachine;   // comes here every second;
            function    GetConnected() : boolean;
		public
			procedure	WMMessageHub( var wm : TMessage ); // message WM_Hub;
			function	InitConnection( links : asLink;
							const MyName, PipeServerName, TCPServerName : string;
							mode : aTCPMode; serverl : TStringList; port : integer ) : boolean;
			procedure	Disconnect( shutdown : boolean = false );
			function	ClientCount : int;
			function	GetError : int;
			function	RegisterReader( const rdr : aReader ) : integer;
			procedure	DeRegisterReader( const rdr : aReader );
			function	Broadcast( const mesg : string; lid : apLinkID = nil; mType : aPktType = pkPassThrough ) : integer;
			property	Master : boolean  read mMaster;
			property	Error : int  read GetError;
			property    LocalIP : string  read GetLocalIP;
            property	Connected : boolean  read  GetConnected;
            property	OnConnection : aEventHandler  read mOnConnection  write SetOnConnection;
		end;

implementation

uses
	uPoller { calls statemachine every second }, SysUtils;

{const
	WM_TCP = WM_USER + $123;    // not used
	WM_UDP = WM_USER + $124;
	WM_Pipe = WM_USER + $125; }


constructor  cMessageHub.Create( log : aLogProc = nil );

	begin
	mLog := log;
    // mhWindow := form.Handle;
	mhWindow := AllocateHWnd( WMMessageHub ); //  dummy window so I can get messages
	mReaders := TList.Create;
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
        FreeAndNil( mTCP );
        FreeAndNil( mUDPserver );
        FreeAndNil( mPipe );
        DeallocateHWnd( mhWindow );
        if mReaders <> nil then  for p in mReaders do  FreeMem( apReader( p ) );
        mReaders.Free;
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
	if mPipe <> nil then  result := result + mPipe.ClientCount;
    if mTCP <> nil then  result := result + mTCP.ClientCount;
	end;


function    cMessageHub.GetLocalIP : string;

	begin
    result := '';
    if mTCP <> nil then   result := mTCP.mLocalIP;
    end;


function   cMessageHub.GetError : int;

	begin
	result := 0;
	if mPipe <> nil then  result := mPipe.Error;
	end;


procedure   cMessageHub.SetOnConnection( handler : aEventHandler );

	begin
    mOnConnection := handler;
    if mTCP <> nil then  begin
    	mTCP.mOnConnection := handler;
        if Connected then  handler();
	    end;
    end;


procedure   cMessageHub.Disconnect( shutdown : boolean );

	var
		s : string;
	begin
    if mTCP <> nil then  mTCP.Disconnect( shutdown );

	if mPipe <> nil then  begin
		if shutdown then
    	s := MesgShutDown  else  s := '<Disconnect/>';
    	mPipe.Broadcast( s, -1, pkDisconnect );
	    end;
	end;


procedure	cMessageHub.WMMessageHub( var wm : TMessage );  // local message catcher

	var
		mp : apPacket;
		xml : string;
		lid : aLinkID;
	begin
	if ( wm.WParam = Ord( alPipe ) ) and not mPipe.mShutdown then  begin
		if ( mPipe <> nil ) and ( mPipe.oInQ.Count > 0 ) then  begin
			mp := mPipe.oInQ.Read();
			if ( mp.pkType = pkPassThrough ) or ( mp.pkType = pkDisconnect ) then  begin
				xml := mp.ToString;
				lid.fLink := alPipe;   lid.fID := mp.pkSrcID;
				Broadcast( xml, @lid );
				end;
			mPipe.FreeMesg( mp );
			end;
		end
	else if ( wm.WParam = Ord( alTCP ) ) and not mTCP.mShutdown then  begin
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
	else if ( wm.WParam = Ord( alUDP ) ) then  begin
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
	end;


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
    if mUDPserver = nil then  begin
        try  mUDPserver := cUDP.Create( mPort, mhWindow, WM_Hub, mLog );
        except    // eg port in use
            mUDPserver := nil;
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

    mServerIPs.Free;    mServerIPs := serverl;
    if mServerIPs = nil then  mServerIPs := TStringList.Create;
    if mServerIPs.IndexOf( LocalHost ) < 0 then   mServerIPs.Insert( 0, LocalHost );   // put local host at top of list

    mMode := mode;
    mPort := port;
	if alLocal in links then  begin
		if mReaders = nil then  mReaders := TList.Create;
		mMaster := true;
		end;

	if alPipe in links then  begin  // preserve pipe on tcp reconfiguration
		// mPipe.Free;
        if mPipe = nil then  begin
			mPipe := cPipe.CreatePipe( MyName, mPipeServerName, mhWindow, WM_Hub );
        	{if mode = tcpCanClient then  Sleep( 300 )  // wait for a connection chatter
            else}  //Sleep( 100 );
            end
        else  mPipe.Clear;
        mPipe.Connect( true );
        Sleep( 100 );
		mMaster := mPipe.Master;
        if not ( alTCP in links ) and mPipe.Connected and Assigned( mOnConnection ) then  mOnConnection();

		end;

	if alTCP in links then  begin
        useTCP := mMode <> tcpNone;
        if ( mMode = tcpCanClient ) and ( mPipe <> nil ) and ( mPipe.ClientCount > 0 ) then  begin
            useTCP := false;  // already pipe connected so don't bother
	        end;

        if useTCP then  begin    // todo
            mMaster := false;    // may set back to true if cannot client in
            TryCreateUDP;
            if mTCP = nil then  mTCP := cTCP.Create( port, mhWindow, WM_Hub, mLog );
            mTCP.mOnConnection := mOnConnection;
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
    if mPipe <> nil then  begin
    	if mPipe.Connected then  result := true;
	    end;
    if mTCP <> nil then  begin
        if mTCP.ClientCount > 0 then  result := true;
    	end;
    end;


procedure  cMessageHub.SetNewState( st : aTCPState );

	begin
    mTCPState := st;
    mStartState := Seconds;
    mRetries := 0;
    end;


procedure  cMessageHub.BeMaster;

	begin
    mTCP.InitConnection( '', ctServer );
    TryCreateUDP;
    if mUDPserver <> nil then  begin
        mUDPserver.IamServerName := mMyName;
        end;
    mMaster := true;
    SetNewState( tsPassThrough );
    end;


procedure  cMessageHub.StateMachine;   // comes here every second - see poller;

    var
    	x : int;
        con, done : boolean;

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
                    if ( mUDPserver <> nil ) and ( mUDPserver.ServerName <> '' ) and ( mUDPserver.ServerName = mTCPServerName ) then  begin
                        if mTCP.InitConnection( mUDPserver.ServerIP, ctClient ) then  SetNewState( tsPassThrough )
                        else  SetNewState( tsWaitServerList );
                        FreeAndNil( mUDPserver );  // not master so don't hog this port
                        end
                    else if mRetries < 2 then  begin
                        Inc( mRetries );
                        if mUDPserver <> nil then  mUDPserver.RequestServer( mTCPServerName );
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
                            if mTCP.InitConnection( mServerIPs[ x ], ctClient ) then  begin
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
                    if Seconds - mStartState > mTCP.ClientCount * 5 then  begin   // periodic server announcement
                        mStartState := Seconds;
                        if mMaster then  begin
                        	TryCreateUDP;
                            if mUDPserver <> nil then  begin
                            	if ( mUDPserver.ServerIP <> '' ) and
                                    ( mUDPserver.ServerIP <> mTCP.mLocalIP ) then  begin
                                    mTCPState := tsNone;  // restart if someone else serving as well !
                                    LogEr( erForcedTCPReconfiguration, 'Forced TCP Reconfiguration' );
                                    end
                                else  mUDPserver.AnnounceServer;
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
	Result := mReaders.Add( rp );
	end;


procedure cMessageHub.DeRegisterReader( const rdr : aReader );

	var
		x : integer;
		rp : apReader;
	begin
	if mReaders <> nil then  begin
		for x := 0 to mReaders.Count - 1 do  begin
			rp := mReaders[ x ];
			// if rp = @rdr then  begin     NFG
            if ( TMethod(rp^).Code = TMethod(rdr).Code ) and ( TMethod(rp^).Data = TMethod(rdr).Data )  then  begin
				mReaders.Delete( x );
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
	if ( mPipe <> nil ) then  begin
		if ( lid = nil ) or ( lid.fLink <> alPipe ) then  e := -1  else  e := lid.fID;
		mPipe.Broadcast( mesg, e );        // to all connected pipes except source
		end;
	if mTCP <> nil then  begin
		if ( lid = nil ) or ( lid.fLink <> alTCP ) then  e := -1  else  e := lid.fID;
		mTCP.Broadcast( mesg, e );         // to all connected pipes except source
		end;
	//	NetCast( nxml, netID );            // Server -> all connected tcp clients   or client to server
	end;


function   cMessageHub.Broadcast( const mesg : string; lid : apLinkID = nil; mType : aPktType = pkPassThrough ) : integer;

	// organise the info into aNotice and tell everyone on the list of readers
	var
		i, e : integer;
		p : apReader;
	begin
    result := 0;
	if mReaders <> nil then begin
    i := mReaders.Count - 1;	//i := 0;
    	result := i + 1;
		if ( lid = nil ) or ( lid.fLink <> alLocal ) then  e := -1  else  e := lid.fID;
		while ( i >= 0 ) and ( i < mReaders.Count ) do  begin  // while i < mReaders.Count do  begin
			try   begin
				if e <> i then  begin
					p := mReaders[ i ];
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
