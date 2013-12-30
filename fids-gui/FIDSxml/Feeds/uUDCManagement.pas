unit uUDCManagement;

{
 (C) Digital Images

this unit handles the (very messy) comings and goings of the UDC farm :-
1) Listed keypad server UDCs are initialized ( aSIP host IP )
2) OnUDCHostsReady action is launched - ie safe to init keypads now
3) listed display UDCs are initialized see 1)
4) repeated Broadcast UDC init starts ( aSIP and aTime ) - every 20 seconds newly turned on UDCs can join the group.
	a newly on UDC sends a tagRefreshUDC message to the formatter.
5) every second a TCP connection is attempted, to the next listed UDC, on port 80 of the UDCs to confirm their presence.
	the <NotResponding> section in <SystemSettings> is maintained to reflect current UDC farm state.
}

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, Contnrs, Generics.Collections, SysUtils,
  Windows, Messages,
  IdGlobal, IdBaseComponent, IdComponent, IdTCPServer, IdContext, IdUDPServer, IdUDPBase,
  IdSocketHandle, IdTCPClient,
  StdCtrls, ExtCtrls, HTTPProd, IdCustomTCPServer, uGT, uMessageHub, uMirrorDB, uDbTree,
  uFidsTags, uFlight, uCheckInControl, uUDC, uUDCTerminalServer, uPacket, uStringGrid;

const
    WM_UDC_Mgt = WM_USER + $252;

type
	apLog = procedure( er : int; const txt : string ) of object;
    apWrLog  = procedure( const txt : string ) of object;
    aUserInterfaceType = ( uiNone, uiGate, uiCIC, uiUDCDisplay );
    asUserInterfaceType = set of aUserInterfaceType;
    aProc = procedure of object;
    aUDCState = ( usNone, usWaitCallIn, usCalledIn, usNotResponding );

    aTerminalServerPkt = record
        SrcIP : string;
        Response : boolean;
        TimeToRespond : int; // mSec
	    end;
    apTerminalServerPkt = ^ aTerminalServerPkt;


    aContext = record    // RO info applicable to all cUdcStatus
        Log : apWrLog;
        Data : cMirrorDB;
        ReqID : string;
        Port : int;
        UDC : cUDCTerminalServer;
        Client : TIdTCPClient;
        List : TStringList;  // actually a cUDCManagement
    	end;
    apContext = ^ aContext;

	cUdcStatus = class
		constructor  CreateInList( const name, format, ip : string; cont : apContext;  UI : aUserInterfaceType );
        private
            mContext : apContext;
            mName, mIP, mFormat : string;
            mSec : int;
            mResponseTime, mMaxResponseTime : int;  // mSec
            mUseCount : int;
            mUITypes : asUserInterfaceType;
            mNotResponding : boolean;
            mWaitingForResponse : boolean;
            mAnnounced : boolean;
            procedure  SendHostIP;
            procedure  SetResponseTime( rt : int );
			// procedure Connect;
			function  NotRespondPath : string;
			procedure AddEntry;  // DB update
			procedure DeleteEntry;  // DB update
        	procedure SetNotResponding( s : boolean );
        	procedure SetWaitingForResponse( s : boolean );
        public
            property  NotResponding : boolean  read  mNotResponding  write SetNotResponding;
            property  WaitingForResponse : boolean  read  mWaitingForResponse  write SetWaitingForResponse;
            property  ResponseTime : int  read mResponseTime  write SetResponseTime;
	    end;

	cUDCManagement = class ( TStringList ) // of cUdcStatus
    	constructor  Create( log : apLog; data : cMirrorDB; reqID : string );
        destructor   Destroy;  override;
        private
            mDB : cMirrorDB;
            oUDC : cUDCTerminalServer;
            oInQ : cLockedQueue;
            oTCPServer : TIdTCPServer;
            oTCPClient : TIdTCPClient;
            oFlight : cFlight;
            oCIC : cCheckInControl;
            pLog : apLog;
            pOnUDCHostsReady : aProc;
            mLocalIP : string;
            mContext : aContext;
            mNextUDCindex : int;
			mhWindow : HWND;
            mState : ( sNone, sFirstPassKeyPads, sFirstPassDisplays, sPolling );
			mPrevMin: word;
			mKPinit: Boolean;
			procedure ReadyForKeypads;
			procedure WMHandler( var wm : TMessage );  // Main thread only here
			procedure AddAllUDCs;
            procedure FirstPass;  // aToPoll
			procedure PollUDCs;   // aToPoll
            procedure WrLog( const txt : string );
            procedure StartTCPServer;
            procedure OnTCPConnect( AContext: TIdContext );   // all TCP threads
            procedure OnTCPExecute(AContext: TIdContext);   // all TCP threads
            //procedure OnTCPDisconnect( AContext: TIdContext );      // all TCP threads
        public
            procedure  Initialize;
            procedure  ShowGrid( grid : cStringGrid );
        	property   OnUDCHostsReady : aProc read pOnUDCHostsReady  write pOnUDCHostsReady;
    	end;

implementation

uses
	uFeedMain, uUtils, uPoller, uXmlParser,
   	IdStack, IdException, IdExceptionCore;

const
	UDCPort = 9200;           // UDC UDP display traffic
    UDCSerialDataPort = 9201; // UDP terminal server incoming
	UDCHostTCPPort = 9201;    // TCP allerter incoming
    UDCHTTPPort = 80;		  // UDC browser i/f can be polled
    LocalHost = '127.0.0.1';


type
	cUDCPoller = class( TThread )
        constructor  Create( OutQ : cLockedQueue; list : cUDCManagement );
    	private
            oTCPClient : TIdTCPClient;
            mOutQ: cLockedQueue;
            mList: cUDCManagement;
            mNextUDCindex : int;
    mConnected: Boolean;
			procedure  Connection( Sender : Tobject );
        	procedure  Execute;   override;
        end;

var
	Shutdown : boolean = false;
    ThreadCount : int = 0;


// __________________________ cUDCPoller THREAD ________________________________

// was forced to create a separate poller thread because Vista and Win7 don't support a 50mSec connection timeout

constructor  cUDCPoller.Create( OutQ : cLockedQueue; list : cUDCManagement );

    begin
    mOutQ := OutQ;
    mList := list;
    FreeOnTerminate := true;
    inherited Create( false );
    end;


procedure  cUDCPoller.Connection( Sender : Tobject );

	begin
    mConnected := true;
    end;


procedure  cUDCPoller.Execute;

    // polls UDC every minute or more by attempting a connection to port 80 - UDP polling not supported on UDC4
    // NOTE the list (cUDCManagement) is not locked - assumes no insert or delete during polling
    var
    	us : cUdcStatus;
        // resp : boolean;
        pp : apTerminalServerPkt;
        start : cardinal;
	begin
    InterlockedIncrement( ThreadCount );     pp := nil;
    try begin
        if mList.Count > 0 then  begin
            oTCPClient := TIdTCPClient.Create( nil );
            oTCPClient.Port := UDCHTTPPort;
            oTCPClient.OnConnected := Connection;
            while not Shutdown do  begin
                if mNextUDCindex < mList.Count then  begin
                    us := mList.Objects[ mNextUDCindex ] as cUdcStatus;   // only sample point from list - should be safe
                    if not us.WaitingForResponse and not us.NotResponding then begin  // us.Connect;
                        start := GetTickCount;
                        if pp = nil then  New( pp );  // each connection or failure sends a packet back to main thread via mOutQ
                        try try
                            pp.SrcIP := us.mIP;
                            pp.TimeToRespond := 0;
                            oTCPClient.Host := us.mIP;
                            mConnected := false;
                            oTCPClient.Connect;
                        except
                            mConnected := false;    // failed to connect
                            end;
                        finally
                    		if mConnected then  pp.TimeToRespond := GetTickCount - start;  // mSec to connect
                            oTCPClient.Disconnect;
                            end;
                        end;
                    end;
            	Sleep( 1000 );  // every second will do
                if Shutdown then  break;

                if pp <> nil then  begin  // pending response message waiting
                	pp.Response := mConnected;   // pick up connected event
                    mOutQ.PostPointer( pp );   // reader is cUDCManagement.WMHandler
                    pp := nil;
                    end;

                Inc( mNextUDCindex );
				if mNextUDCindex >= Max( mList.Count, 20 ) then  mNextUDCindex := 0;  // todo 60  cycle through list no more than every minute
                //if mNextUDCindex >= mList.Count then  mNextUDCindex := 0;
                end;  // while not shutdown

            oTCPClient.Disconnect;
            oTCPClient.Free;
            end;
        end;
    finally  InterlockedDecrement( ThreadCount );  end;
    end;

// ______________________ cUdcStatus   main thread______________________________


constructor  cUdcStatus.CreateInList( const name, format, ip : string; cont : apContext;  UI : aUserInterfaceType );

    var
    	x : int;
        us : cUdcStatus;
	begin                        // warning somwhat unconventional create - creates into list, maybe
    if not cont.List.Find( ip, x ) then  begin    //  new IP
        cont.List.AddObject( ip, self );  // put myself into the list
        mContext := cont;
        mName := name;
        mFormat := format;
        mIP := ip;
        end
    else  begin         // IP already in list so re-use old object
        us :=  cont.List.Objects[ x ] as cUdcStatus;
        Free;
        self := us;  // ie no new object returned
        mName := mName + ', ' + name;
        if format <> '' then  mFormat := format;
	    end;
    Include( mUITypes, UI );
    if UI = uiUDCDisplay then  Inc( mUseCount );   // 0 or > 1 would indicate problem - ie unused or multiply used UDCs
    end;


procedure  cUdcStatus.SetResponseTime( rt : int );

	begin
    mResponseTime := rt;
    if rt > mMaxResponseTime then  mMaxResponseTime := rt;
    end;


procedure  cUdcStatus.SendHostIP;

	begin
    mSec := Seconds;
    mContext.UDC.SetHost( mIP, UDCHostTCPPort );
    end;


function  cUdcStatus.NotRespondPath : string;

	begin
    result := pathNotResponding + 'UDC|' + mIP + '|';
    end;


procedure cUdcStatus.AddEntry;

    var
    	r : string;
	begin
    if mContext.Data.GetNode( NotRespondPath ) = nil then  begin
    	StartRequestNew( mIP );
        AddToRequestNew( TimeStamp );
        r := EndRequestNew( pathNotResponding + 'UDC|', '', '', mContext.ReqID );
        mContext.Data.SendRequest( r );
        mContext.Log( 'No response from UDC ' + mIP );
    	end;
    end;


procedure cUdcStatus.DeleteEntry;

    var
    	r : string;
	begin
    if mFormat <> '' then  begin  // UDC is assigned to a named format
        Poller.OnTimeOut( 10,     // wait a second then tell formatter to refresh UDC(s)
            procedure  begin
            mContext.Data.SendRequest( '<'+tagRefreshUDC+'> ' + mIP + ' </'+tagRefreshUDC+'>' );   // signal to formatter
            end  );    // UDC needs a moment to compose itself
        end;
    if mContext.Data.GetNode( NotRespondPath ) <> nil then  begin
        r := FormatDelete( NotRespondPath, mContext.ReqID );
        mContext.Data.SendRequest( r );
        mContext.Log( 'Operating UDC ' + mIP );
    	end;
    end;


procedure cUdcStatus.SetNotResponding( s : boolean );

	begin
    if s <> mNotResponding then  begin
        mNotResponding := s;
        if s then  begin
        	AddEntry;
            mAnnounced := false;
            end
        else  DeleteEntry;
        mSec := Seconds;
        end;
    end;


procedure cUdcStatus.SetWaitingForResponse( s : boolean );

	begin
    mSec := Seconds;
    if s <> mWaitingForResponse then  mWaitingForResponse := s;
    end;


{procedure cUdcStatus.Connect;

	var
    	t : int;
	begin
    WaitingForResponse := true;
    mContext.Client.Disconnect;
    mContext.Client.Host := mIP;
    mContext.Client.ConnectTimeout := 50;  // 50 mSec to connect   at 1/second worst case ~5% CPU
    try  begin
        try begin
            mContext.Client.Connect;
            if mContext.Client.Connected then  begin
                WaitingForResponse := false;
                NotResponding := false;
                end;
            end;
        except
            Inc( t );
        	end;  // swallow timeout exception
        end;
    finally
    	mContext.Client.Disconnect;
        end;
    end; }


// __________________________ cUDCManagement ___________________________________


constructor  cUDCManagement.Create( log : apLog; data : cMirrorDB; reqID : string );

	begin
    inherited Create();
    Sorted := true;
    Duplicates := dupIgnore;  // don't want to duplicate IPs

    mDB := data;
    mContext.Data := data;
    pLog := log;
	mhWindow := AllocateHWnd( WMHandler ); //  dummy window so I can get messages
    oInQ := cLockedQueue.Create( mhWindow, WM_UDC_Mgt );
    mContext.List := self;
    oUDC := cUDCTerminalServer.Create( false );
    mContext.UDC := oUDC;
    oTCPClient := TIdTCPClient.Create( nil );
    oTCPClient.Port := UDCHTTPPort;
    mContext.Client := oTCPClient;
    mContext.Log := WrLog;
    mContext.ReqID := reqID;
    end;


destructor   cUDCManagement.Destroy;

	begin
    oTCPServer.Active := false;
    while oTCPServer.Active do  Sleep( 10 );
    FreeAndNil( oTCPServer );
    FreeAndNil( oUDC );
    FreeAndNil( oInQ );
    FreeAndNil( oFlight );
    FreeAndNil( oCIC );
    DeallocateHWnd( mhWindow );  mhWindow := 0;
    end;



procedure	cUDCManagement.  ReadyForKeypads;

	begin
    if Assigned( pOnUDCHostsReady ) and not ( mKPinit )  // might be a few of them at once
         then  begin  // UDC moonlights as a terminal server
        mKPinit := true;
        Poller.OnTimeOut( 40,
            procedure  begin
            mKPinit := false;
            pOnUDCHostsReady();
            end );  // reinit keypads
        end;
    end;                     // without the delay crashes the UDC


procedure	cUDCManagement.WMHandler( var wm : TMessage );

    var
        pp : apTerminalServerPkt;
        us : cUdcStatus;
        x : int;
        DoInit : boolean;
	begin                                  // handle UDC response messages
    if wm.Msg = WM_UDC_Mgt then  begin
        while oInQ.Count > 0 do  begin
        	pp := oInQ.ReadPointer;
            if Find( pp.SrcIP, x ) then  begin
                us := GetObject( x ) as cUdcStatus;
                us.WaitingForResponse := false;
                DoInit := us.NotResponding and pp.Response;
                us.NotResponding := not pp.Response;
                us.ResponseTime := pp.TimeToRespond;
                if ( mState = sPolling ) and DoInit then  begin
//                	if us.mFormat <> '' then  begin  // UDC is assigned to a named format
//                    	Poller.OnTimeOut( 10,
//                        	procedure  begin
//                        	mDB.SendRequest( '<'+tagRefreshUDC+'> ' + us.mIP + ' </'+tagRefreshUDC+'>' );   // signal to formatter
//                            end  );    // UDC needs a moment to compose itself
//                        us.mAnnounced := true;
//                        end;
                    if ( [ uiGate, uiCIC ] * us.mUITypes <> [] ) then  begin  // UDC moonlights as a terminal server
                        ReadyForKeypads          // force a re-initialization of key apd stuff
                        end;
                    end;
            	end
            else  cUdcStatus.CreateInList( '', '', pp.SrcIP, @ mContext, uiNone );  // poor lost UDC
            Dispose( pp );
        	end;
    	end;
    end;


procedure  cUDCManagement.WrLog( const txt : string );

	begin
    if Assigned( pLog ) then  pLog( 0, 'Device Mgt.  ' + txt );
    end;


procedure    cUDCManagement.AddAllUDCs;

    var
        pt, pDevices, pFormats, pPage, pIPs : apNode;
        f, d : int;
        ip : string;
        IPs : TList;
        p : pointer;
	begin        // walk displayconfig to find all udc IPs
    pt := mDB.GetNode( pathFormats );   f := -1;
    while EachSubNode( pt, f, pFormats ) do  begin    // for each format
        pDevices := FindName( pFormats, tagDevices );
    	d := -1;
        while EachSubNode( pDevices, d, pPage ) do  begin   // for each page
            if NodeName( pPage ) = 'Page' then  begin
                pIPs := FollowPath( '|UDC|IPs|', pPage );
                if pIPs <> nil then  begin
                    try IPs := ParseIPs( NodeContent( pIPs ) );
                    except  FreeAndNil( IPs );  end;
                    if IPs <> nil then  begin
                        for p in IPs do  begin
                            ip := IPtoStr( Integer( p ) );
                            cUdcStatus.CreateInList( NodeName( pFormats ) + ' ' + NodeContent( pPage )
                            	, NodeName( pFormats ), ip, @ mContext, uiUDCDisplay );
                        	end;
                    	end;
                	end;
            	end;
        	end;
        end;
    end;


procedure    cUDCManagement.PollUDCs;          // comes here once a second

    var
		Hour, Min, Sec, MSec : word;
	begin     // every 20 seconds broadcast host IP to catch UDCs just powered-up
    if mState = sPolling then  begin
		DecodeTime( Now, Hour, Min, Sec, MSec );      // send time, host, etc on minute change and at 20 and 40 seconds
        if ( Min <> mPrevMin ) or ( Sec = 20 ) or ( Sec = 40 )  then  begin   // not precise but good enough for polling
            mPrevMin := Min;
    		oUDC.BroadcastHost( UDCHostTCPPort, Hour, Min );
        	end
    	end;
    end;


procedure    cUDCManagement.FirstPass;

    var
        us : cUdcStatus;   // initial run through all listed UDCs
        x : int;
        dis : string;
	begin    // every tenth of a second invite another UDC to log in
    if mState = sFirstPassKeyPads then  begin     // do keypads first
        if mNextUDCindex < Count then  begin
            us := Objects[ mNextUDCindex ] as cUdcStatus;
            Inc( mNextUDCindex );
            if not us.WaitingForResponse then  begin
            	us.WaitingForResponse := true;
                us.SendHostIP;  // ie invite a connection
            	end;
        	end;
        if mNextUDCindex >= Count then  begin   // all keypad UDCs initialised
            if Assigned( pOnUDCHostsReady ) then  pOnUDCHostsReady();  // can init keypads now that UDCs know their host IP
            AddAllUDCs;
            mState := sFirstPassDisplays;
        	end;
    	end
    else if mState = sFirstPassDisplays then  begin  // tell all UDCs their host IP - also one at a time
        if mNextUDCindex < Count then  begin
            us := Objects[ mNextUDCindex ] as cUdcStatus;
            Inc( mNextUDCindex );
            if not us.WaitingForResponse then  begin
            	us.WaitingForResponse := true;
                us.SendHostIP;  // ie invite a connection
            	end;
        	end
        else  if mNextUDCindex >= Count then  begin   // all listed UDCs initialised
            mState := sPolling;
            ReadyForKeypads;  // dust has settled on UDCs so safe to kick off keypads
            for x := 0 to Count - 1 do  begin
                us := Objects[ x ] as cUdcStatus;
                if us.WaitingForResponse then  begin   // tidy up initial state
                    us.WaitingForResponse := false;
                    us.NotResponding := true;
                    end;
                end;
            if mDB.GetNode( '|SystemConfig|ActivePollUDCs|' ) <> nil then  begin   // todo may want to only disable client poll for debug
                Poller.PollMe( PollUDCs );  // now poll a UDC every second
                Poller.OnTimeOut( 3, procedure  begin  Poller.UnPollMe( FirstPass );  end ); // and don't come here any more
                cUDCPoller.Create( oInQ, self );
                end
            else  dis := ' polling disabled';
            WrLog( 'UDC monitor has ' + IntToStr( mNextUDCindex ) + ' listed UDCs' + dis );
            end;
        end;
    end;


type       // sadly must be global to get RTTI
    aCol = ( cUDC, cFormat, cUsed, cDescription, cStatus, cSec, cRespTime );  // column headings and internal name

procedure  cUDCManagement.ShowGrid( grid : cStringGrid );

    var
    	x : int;
        us : cUdcStatus;
        cell : string;
        c : aCol;
	begin
    if grid <> nil then  begin
        for c := Low( aCol ) to High( c ) do  begin                  // could clear grid first but no need here
            if not ( c in [ cUsed, cSec, cRespTime ] ) then  begin   // default is right
            	grid.JustifyCol[ Ord( c ) ] := juLeft;
                end;
            grid.SetText( Ord( c ), 0, EnumToStr( Ord( c ), TypeInfo( aCol ) ) );   // set headings
            end;
        for x := 1 to Count do  begin
            us := Objects[ x - 1 ] as cUdcStatus;
            grid.SetText( Ord( cUDC ), x, us.mIP );
            grid.SetText( Ord( cFormat ), x, us.mFormat );
            grid.SetText( Ord( cUsed ), x, IntToStr( us.mUseCount ) );
            grid.SetText( Ord( cDescription ), x, us.mName );
            if us.NotResponding then  cell := 'NOT RESPONDING'
            else if us.WaitingForResponse then  cell := 'waiting'
            else cell := '';
            grid.SetText( Ord( cStatus ), x, cell );
            grid.SetText( Ord( cSec ), x, IntToStr( Seconds - us.mSec ) );
            if not us.NotResponding then  begin
            	grid.SetText( Ord( cRespTime ), x, IntToStr( us.mResponseTime ) + '/' + IntToStr( us.mMaxResponseTime ) );
                end;
            end;
        end;
    end;


procedure  cUDCManagement.Initialize;

    var
        pt, pGates, pCICs, pg : apNode;
        x : int;
        name : string;
	begin
    StartTCPServer;
    mState := sFirstPassKeyPads;

    pt := FollowPath( pathKeyPads_UDC, mDB.GetRoot );
    if pt <> nil then  begin    // if enabled on this job
        pGates := FindName( pt, 'Gates' );
        pCICs := FindName( pt, 'CICs' );
        if ( pGates <> nil ) or ( pCICs <> nil ) then  begin
            if oUDC = nil then  begin  // UDC actually used as terminal server
                oUDC := cUDCTerminalServer.Create( false );
            	end;
        	end;
        x := -1;
        while EachSubNode( pGates, x, pg ) do  begin   // create a cUdcStatus for each Gate entry
            name := 'KP-Gate ' + NodeName( pg );
            cUdcStatus.CreateInList( name, '', ReadContent( pg, 'IP' ), @ mContext, uiGate );
        	end;

        x := -1;
        while EachSubNode( pCICs, x, pg ) do  begin   // create a cKeyPad for each CIC entry
            name := 'KP-CheckIn ' + NodeName( pg );
            cUdcStatus.CreateInList( name, '', ReadContent( pg, 'IP' ), @ mContext, uiCIC );
        	end;
        end;

    //if Count > 0 then  begin
    	Poller.PollMeTenths( FirstPass );  // poll every tenth of a second
	    //end
    // else  ReadyForKeypads;  // no UDC keypads to worry about
    end;


procedure  cUDCManagement.StartTCPServer;

	var
		i : integer;
		Binding : TIdSocketHandle;
    begin
    if oTCPServer = nil then  begin
        try  begin  // be server
            if oTCPServer = nil  then  oTCPServer := TIdTCPServer.Create( nil )
            else                    oTCPServer.Active := false;
            //for i := 0 to GStack.LocalAddresses.Count - 1 do  begin
                // mLocalIP := GStack.LocalAddresses[ 0 ];
                //if mLocalIP <> LocalIP then  break;
                //end;

            oTCPServer.OnConnect := OnTCPConnect; // procedure(AThread: TIdPeerThread) of object;
            oTCPServer.OnExecute := OnTCPExecute; // procedure(AThread: TIdPeerThread) of object;
            //oTCPServer.OnDisconnect := OnTCPDisconnect; // procedure(AThread: TIdPeerThread) of object;
            //mServer.CommandHandlersEnabled := false;  // use onexecute connection
            mLocalIP := GStack.LocalAddresses[ 0 ];

            oTCPServer.Bindings.Clear;
            for i := 0 to GStack.LocalAddresses.Count - 1 do  begin
                Binding := oTCPServer.Bindings.Add;
                Binding.IP := GStack.LocalAddresses[ i ];   // eg 192.168.0.163    :1111
                Binding.Port := UDCHostTCPPort;  // TCPPort;
                end;
            Binding := oTCPServer.Bindings.Add;
            Binding.IP := LocalHost;
            Binding.Port := UDCHostTCPPort;  // TCPPort;

            oTCPServer.Active := true;
            end
        except
            {$ifndef DEBUG }
            Log( stNoConToServer, 'Server ' + Binding.IP + ':' + IntToStr( Binding.Port ) + ' failed to start' );
            {$endif }
            FreeAndNil( oTCPServer );
	        end;
        end;
    end;


//_____________________________ TCP reader threads _____________________________


procedure cUDCManagement.OnTCPConnect( AContext: TIdContext );   // all TCP threads

    var
        pp : apTerminalServerPkt;
    begin
    try  begin
        New( pp );
        pp.SrcIP := AContext.Connection.Socket.Binding.PeerIP;   // post caller ip to main thread
        pp.Response := true;
        pp.TimeToRespond := 0;  // actually no idea
        oInQ.PostPointer( pp );
        end;
    except

    	end;
	WrLog( 'TCP Connection from ' + AContext.Connection.Socket.Binding.PeerIP );  // todo remove after debug
	end;


procedure cUDCManagement.OnTCPExecute(AContext: TIdContext);   // all TCP threads

    begin
    try  begin
        AContext.Connection.Disconnect;
	    end;
    except
        // ignore disconnect exception
    	end;
	end;


//procedure cUDCManagement.OnTCPDisconnect( AContext: TIdContext );      // all TCP threads
//
//	begin
//	//WrLog( 'TCP Disconnect' );
//	end;



end.
