unit uKeyPadServer;

// (C) Digital Images

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, Contnrs, Generics.Collections, SysUtils,
  Messages,
  IdGlobal, IdBaseComponent, IdComponent, IdTCPServer, IdContext, IdUDPServer, IdUDPBase,
  IdSocketHandle,
  StdCtrls, ExtCtrls, HTTPProd, IdCustomTCPServer, uGT, uMessageHub, uMirrorDB, uDbTree,
  uFidsTags, uFlight, uCheckInControl, uUDC, uUDCTerminalServer, uLantronixTerminalServer,
  uPacket;

const
	KeyPadID = 'KPad';
    WM_KeyPad = WM_USER + $251;   // window messages to be passed through

type
	apLog = procedure( er : int; const txt : string ) of object;
    apWrLog  = procedure( const txt : string ) of object;
    aTerminalServer = ( tsNone, tsUDC, tsLantronix, tsUDCPresent );
    aUserInterfaceType = ( uiNone, uiGate, uiCIC );

    aContext = record    // RO info applicable to all sessions
        Log : apWrLog;
        Data : cMirrorDB;
        UDC : cUDCTerminalServer;
        Flight : cFlight;
        CIC : cCheckInControl;
    	end;
    apContext = ^ aContext;

    aTerminalServerPkt = record
        TsType : aTerminalServer;
        SrcIP : string;
        SeqNo : int;
        Drop : int;
        Data : string;
	    end;
    apTerminalServerPkt = ^ aTerminalServerPkt;

    cKeyPad = class
    	constructor  Create( nam : string; cont : apContext; ip, drop : string;
        	port : int;  ts : aTerminalServer; UI : aUserInterfaceType );
        private
            mContext : apContext;
            mDrop : int;
            mDisplay : string;
            mTsType : aTerminalServer;
            mUI : aUserInterfaceType;
            mCICfield : ( cfNone, cfAirline, cfFlights, cfClass, cfService, cfSummary );
            mFlightField : aFlightField;
            mTerminalServer : iTerminalServer;
    		mPort: int;
    mPrevLED: Cardinal;
    mInit: Boolean;
			// procedure  LantronixSend( const dis : string );
			procedure  Transmit( s : string );
            procedure  SetLED( led : cardinal; state : cardinal );
            procedure  SelectLED( led : cardinal );
            procedure  HandleInput( SrcIP : string; rx : TBytes );  // apTSReader
            procedure  StatusNotification( s : aConnectionStatus );  // apNotify
            function   Sel( const path : string; n : int ) : string;
            procedure  WrLog( const txt : string );
            procedure  Initialize;
        public
            mName, mIP : string;
        	procedure  Process( pp : apTerminalServerPkt );
            procedure  Redraw;

    	end;

	cKeyPadServer = class
    	constructor  Create( log : apLog; data : cMirrorDB );
        destructor   Destroy;  override;
        public
            procedure  Initialize;
        private
            oKeyPads : TList< cKeyPad >;
            //oSessionsLock : TMultiReadExclusiveWriteSynchronizer;
            mContext : aContext;
            pLog : apLog;
            oUDC : cUDCTerminalServer;
            oUDPServer : TIdUDPServer;  // to catch UDC as terminal server messages
            oInQ : cLockedQueue;
            oTCPServer : TIdTCPServer;
            oFlight : cFlight;
            oCIC : cCheckInControl;
            mInit : boolean;
    		mLoggedIn: Boolean;
            //mLocalIP : string;

            procedure  WrLog( const txt : string );
            procedure  UDPReadEvent( AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
			procedure  InitTerminalServers( ts : aTerminalServer );

        public
            procedure  WMKeyPad( var wm : TMessage );  // message WM_KeyPad;
            procedure  Redraw;
    	end;


implementation

uses
	uFeedMain, uUtils, ASCII,
   	IdStack, IdException, IdExceptionCore;

const
	UDCPort = 9200;              // default ports
    UDCSerialDataPort = 9201;
	TCPPort = 9201;
    LantronixPort = 3001;
    LocalHost = '127.0.0.1';


constructor  cKeyPad.  Create( nam : string; cont : apContext; ip, drop : string;
        	port : int;  ts : aTerminalServer; UI : aUserInterfaceType );

	begin
    mName := nam;
    mContext := cont;
    mIP := ip;
    mPort := port;
    mTsType := ts;
    mUI := UI;
    if mUI = uiGate then  mFlightField := ffGates
    else if mUI = uiCIC then  mFlightField := ffCheckIns;
    TryStrToInt( drop, mDrop );
    end;


procedure    cKeyPad.WrLog( const txt : string );

	begin
    if Assigned( mContext.Log ) then  mContext.Log( txt );
    end;


procedure    cKeyPad.  Transmit( s : string );

	begin
    if mTsType = tsUDC then  begin
        mContext.UDC.SendSerialPort( mIP, mDrop, s );  //  todo interface ?  - all output through common driver
        end
    else if mTsType = tsLantronix then  begin
        mTerminalServer.Send( s ); // LantronixSend( dis );
        end;
    end;


procedure    cKeyPad.  Redraw;

    var
        dis : string;
    	fl : cFlightList;
        x : int;
	begin
    dis := mDisplay;
    mContext.Flight.Find( fkDepartures, mFlightField, mName );
    if ( mUI = uiGate ) or ( mCICfield = cfNone ) then  begin
        dis := mContext.Flight.Raw[ ffFlight ] + ' ' + mContext.Flight.Presentation[ ffDStatus ] + ' '
        	+ mContext.Flight.Presentation[ ffSTime ] + ' ' + mContext.Flight.Presentation[ ffPorts ];
    	end
    else if mUI = uiCIC then  begin
    	mContext.CIC.Name := mName;
        case mCICfield of
        	cfAirline : dis := mContext.CIC.DB[ cicAirline ];
            cfClass : dis := mContext.CIC.DB[ cicClass ];
            cfService : dis := mContext.CIC.DB[ cicService ];
            cfSummary : begin
            	dis := mContext.CIC.GetCode( '|SystemSettings|Strings|Airlines|', mContext.CIC.DB[ cicAirline ] );
                dis := dis + ' ' + mContext.CIC.GetCode( '|SystemSettings|Strings|CheckInClasses|', mContext.CIC.DB[ cicClass ] );
                dis := dis + ' ' + mContext.CIC.GetCode( '|SystemSettings|Strings|CheckInServices|', mContext.CIC.DB[ cicService ] );
                dis := dis + ' ' + IntToStr( mContext.CIC.NumbFlights ) + ' flts';
	            end;
            cfFlights : begin
                fl := cFlightList.Create( mContext.Data );
                fl.Build( fkDepartures, ffCheckIns, mName );
                dis := '';
                for x := 0 to fl.Count - 1 do  begin
                    mContext.Flight.DbNode := fl[ x ];
            		dis := dis + mContext.Flight.Raw[ ffFlight ] + ' ';
                    end;
                end;
    		end;
        end;
    if dis <> mDisplay then  begin   // if display has changed
    	Transmit( dis );
        mDisplay := dis;
        end;
    end;


function  cKeyPad.  Sel( const path : string; n : int ) : string;

    var                    // select nth item from list
    	pt : apNode;
        list : TStringList;
    begin
    pt := mContext.Data.GetNode( '|SystemSettings|Keypad|' + path );
    list := LoadContentList( pt );
    if ( n >= 0 ) and ( list <> nil ) and ( n < list.Count ) then  result := list[ n ]
    else  result := '';
    list.Free;
    end;


procedure  cKeyPad.  Process( pp : apTerminalServerPkt );

    var
        n, x : int;
        nam, ciList : string;
        ed : boolean;
	begin
    ed := false;
    if Length( pp.Data ) >= 1 then  begin
    	mContext.Flight.Find( fkDepartures, mFlightField, mName );
        if mUI = uiGate then  begin
            if Length( pp.Data ) = 1 then  begin
            	if ( pp.Data >= 'A' ) and ( pp.Data <= 'E' ) then  begin
                	mContext.Flight.Presentation[ ffDStatus ] := Sel( 'Status', Ord( pp.Data[ 1 ] ) - Ord( 'A' ) );
                    ed := true;
	                end
                else if pp.Data >= 'F' then  begin  mDisplay := '?';  end;  // force redraw
                mContext.Log( 'edit Gate ' + mName );
                end;
            end
        else if mUI = uiCIC then begin
            mContext.CIC.Name := mName;
            case pp.Data[ 1 ] of
                'A' : mCICfield := cfAirline;
                'B' : mCICfield := cfFlights;
                'C' : mCICfield := cfClass;
                'D' : mCICfield := cfService;
                'E' : mCICfield := cfSummary;
                'F' : begin  mCICfield := cfNone;  mDisplay := '?';  ed := false;  end;
            	else  begin
                    x := 1;  n := GetInt( pp.Data, x );
                    if ( n > 0 ) and ( x > Length( pp.Data ) ) then  begin  // all decimal entry
                        ed := true;
                        case mCICfield of
                            cfAirline : mContext.CIC.DB[ cicAirline ] := Sel( 'AirLines', n );
                            cfClass : mContext.CIC.DB[ cicAirline ] := Sel( 'Classes', n );
                            cfService : mContext.CIC.DB[ cicAirline ] := Sel( 'Services', n );
                            cfSummary : ed := false;
                            cfFlights : begin
                            	nam := mContext.CIC.GetCode( '|SystemSettings|Strings|Airlines|', mContext.CIC.DB[ cicAirline ] );
                                nam := nam + ' ' + IntToStr( n );
                            	mContext.Flight.Find( fkDepartures, ffFlight, nam );
                                if mContext.Flight.DbNode <> nil then  begin  // have a matching flight name
                                    ciList := mContext.Flight.Presentation[ ffCheckIns ];
                                    ciList := IncludeInList( mName, ciList );
                                    mContext.Flight.Presentation[ ffCheckIns ] := ciList;
                                	end;
	                            end
                            else  ed := false;
                        	end;
                    	end;
                    end;
                end;  // case char
            Redraw;
            SelectLED( Ord( mCICfield ) );
        	end;
        if ed then  begin
            mContext.CIC.SortOutTheHorribleMess;
        	mContext.Log( 'edit CIC ' + mName );
	        end;
    	end;
    end;


procedure  cKeyPad.  HandleInput( SrcIP : string; rx : TBytes );  // apTSReader

    var
        x : int;
        pp : apTerminalServerPkt;
	begin
    if ( Length( rx ) > 2 ) and ( SrcIP = mIP ) then  begin   // parse to look like a UDC packet
        New( pp );
        pp.TsType := tsLantronix;
        pp.SrcIP := SrcIP;
        pp.SeqNo := 0;  // na
        pp.Drop := ( rx[ 0 ] - Ord( '0' ) ) * 10 + rx[ 1 ] - Ord( '0' );
        for x := 2 to Length( rx ) - 2 do  begin   // skip drop address and terminal etx
            pp.Data := pp.Data + Chr( rx[ x ] );
            end;
        Process( pp );
        Dispose( pp );
    	end;
    end;


procedure  cKeyPad.  StatusNotification( s : aConnectionStatus );  // apNotify

	begin
    mContext.Log( EnumToStr( Ord( mUI ), TypeInfo( aUserInterfaceType ) ) + ' '
    	+ mName + ' ' + mIP + ' ' + EnumToStr( Ord( s ), TypeInfo( aConnectionStatus ) ) );
    //Label1.Caption := EnumToStr( Ord( s ), TypeInfo( aConnectionStatus ) );  // apNotify;
    end;


procedure  cKeyPad.  SetLED( led : cardinal; state : cardinal );

	begin
    if ( led <= 6 ) and ( state <= 2 ) then  begin
    	Transmit( ESC + 'h' + IntToStr( led ) + IntToStr( state ) + ETX );  //
	    end;
    end;
// <ESC>h<light><action><terminator>  LED control   light '0'=all '1'..'6'   action '0'= off '1'=on '2'=toggle


procedure  cKeyPad.  SelectLED( led : cardinal );

	begin
    if ( led >= 1 ) and ( led <= 6 ) and ( led <> mPrevLED ) then  begin
        SetLED( mPrevLED, 0 );   // turn off the old one
        SetLED( led, 1 );        // turn on the new one
        mPrevLED := led;
    	end;
    end;


procedure  cKeyPad.  Initialize;

	var
    	key : int;
        buf : string;
		//Hour, Min, Sec, MSec : word;
    begin
    if mTsType = tsUDC then  begin
        //DecodeTime( Now, Hour, Min, Sec, MSec );
        // mContext.UDC.BroadcastHost( mContext.Port, Hour, Min );  do from udc mgt
        mContext.UDC.SetSerialPort( mIP, mPort );
	    end
    else if mTsType = tsLantronix then  begin
    	if mTerminalServer = nil then  begin
        	mTerminalServer := TerminalServerInitialize( mIP, mPort, HandleInput, StatusNotification, mdPollKeypad );
	        end;
    	end;
    if not mInit then  begin
    	mInit := true;
        for key := 1 to 6 do  begin    // initialize function keys  RS => send without waiting for <Enter> key
            buf := ESC + 'm0' + IntToStr( key ) + Char( Ord( 'A' ) - 1 + key ) + RS + ETX;  //;
            Transmit( buf );
            end;
	    end;
    SetLED( 0, 0 );  // turn all the leds off
    mDisplay := '';   mCICfield := cfAirline;
    SelectLED( Ord( mCICfield ) );
    Redraw;
    // some older type of keypad may require the function keys to be initialised - cannot test with current keypad
    end;
// <ESC>m<number><text><terminator>  number '01'..'06'     eg <ESC>m01End<RS><STX>


//___________________ cKeyPadServer ____________________________________________

constructor  cKeyPadServer.Create( log : apLog; data : cMirrorDB );

	begin
    pLog := log;
    oKeyPads := TList< cKeyPad >.Create;
    mContext.Log := WrLog;
    mContext.Data := data;
    oInQ := cLockedQueue.Create( fFeedMain.Handle, WM_KeyPad );
    oFlight := cFlight.Create( data, KeyPadID );
    mContext.Flight := oFlight;
    oCIC := cCheckInControl.Create( data, KeyPadID );
    mContext.CIC := oCIC;
    end;


destructor   cKeyPadServer.Destroy;

    var
    	kp : cKeyPad;
	begin
    if oTCPServer <> nil then  begin    // kill and free server
        oTCPServer.Active := false;
        while oTCPServer.Active do Sleep( 10 );
        FreeAndNil( oTCPServer );
    	end;
    for kp in oKeyPads do  kp.Free;
    FreeAndNil( oKeyPads );
    FreeAndNil( oUDC );
    FreeAndNil( oFlight );
    FreeAndNil( oCIC );
    end;


procedure  cKeyPadServer.WrLog( const txt : string );

	begin
    if Assigned( pLog ) then  pLog( 0, 'KeyPad  ' + txt );
    end;


procedure	cKeyPadServer.WMKeyPad( var wm : TMessage );  // message WM_KeyPad;

    var
        pp : apTerminalServerPkt;
        kp : cKeyPad;
	begin
    while oInQ.Count > 0 do  begin
        pp := oInQ.ReadPointer();
        for kp in oKeyPads do  begin
        	if kp.mIP = pp.SrcIP then  kp.Process( pp );
	        end;
        Dispose( pp );
    	end;
    end;


procedure	cKeyPadServer.Redraw;

    var
        kp : cKeyPad;
	begin
    for kp in oKeyPads do  begin
        kp.Redraw;
        end;
    end;


//___________________________ UDP READER THREAD ONLY ___________________________

procedure  cKeyPadServer.UDPReadEvent( AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);

    var
        pp : apTerminalServerPkt;
        len, i : byte;
    begin
    try  begin
    	len := AData[ 8 ];
        if len + 9 = Length( AData ) then  begin
            New( pp );
            pp.TsType := tsUDC;
            pp.SrcIP := ABinding.PeerIP;
            pp.SeqNo := AData[ 7 ] + 256 * AData[ 6 ];  // big endian
            pp.Drop := AData[ 10 ] - Ord( '0' );  // works for drop 1..9
            for i := 11 to Length( AData ) - 1 do  begin   // copy data bytes into string
                pp.Data := pp.Data + Chr( AData[ i ] );
            	end;
            oInQ.PostPointer( pp );
        	end;
    	end;
    except

    	end;
    end;
//______________________________________________________________________________


procedure   cKeyPadServer.  InitTerminalServers( ts : aTerminalServer );

    var
        pt, pGates, pCICs, pg : apNode;
        x, port : int;
        name, base : string;
	begin
    if ts = tsUDC then  base := '|SystemConfig|KeyPads_UDC|'
    else if ts = tsLantronix then  base := '|SystemConfig|KeyPads_Lantronix|';
    pt := FollowPath( base, mContext.Data.GetRoot );
    if pt <> nil then  begin    // if enabled on this job and list not initialised
        if not mLoggedIn then  begin
        	fFeedMain.LogIn( KeyPadID, SysPW, KeyPadID );  // login to enable db updates from keypads
            mLoggedIn := true;
	        end;
        if ts = tsUDC then             port := UDCSerialDataPort
        else if ts = tsLantronix then  port := LantronixPort;
        pGates := FindName( pt, 'Port' );
        if pGates <> nil then  TryStrToInt( NodeContent( pGates ), port );  // over ride default port
        pGates := FindName( pt, 'Gates' );
        pCICs := FindName( pt, 'CICs' );
        if ( pGates <> nil ) or ( pCICs <> nil ) then  begin
        	if ts = tsUDC then  begin
                if oUDC = nil then  begin  // UDC actually used as terminal server
                    oUDC := cUDCTerminalServer.Create( false );   // no logging
                    mContext.UDC := oUDC;
                    end;
                if oUDPServer = nil then  begin
                    oUDPServer := TIdUDPServer.Create;
                    oUDPServer.DefaultPort := port;
                    oUDPServer.OnUDPRead := UDPReadEvent;
                    oUDPServer.BroadcastEnabled := true;   // not actually used
                    oUDPServer.Active := true;
                    end;
            	end;
            end;
        x := -1;
        while EachSubNode( pGates, x, pg ) do  begin   // create a cKeyPad for each Gate entry
            name := NodeName( pg );
            oKeyPads.Add( cKeyPad.Create( name, @ mContext, ReadContent( pg, 'IP' ), ReadContent( pg, 'Drop' ), port, ts, uiGate ) );
            end;

        x := -1;
        while EachSubNode( pCICs, x, pg ) do  begin   // create a cKeyPad for each CIC entry
            name := NodeName( pg );
            oKeyPads.Add( cKeyPad.Create( name, @ mContext, ReadContent( pg, 'IP' ), ReadContent( pg, 'Drop' ), port, ts, uiCIC ) );
            end;
        end;
    end;


procedure  cKeyPadServer.  Initialize;

    var
        kp : cKeyPad;
	begin
    if not mInit then  begin
    	mInit := true;  // once is enough
        InitTerminalServers( tsUDC );
        InitTerminalServers( tsLantronix );

    	if oKeyPads.Count > 0 then  WrLog( 'Server has ' + IntToStr( oKeyPads.Count ) + ' listed keypads' );
    	end;

    for kp in oKeyPads do  begin
        kp.Initialize;
        end;
    end;
{
DI keypad setup :-

vw	4
tm	1	block mode
ma	01
td	0
br	2	9600
df	4   8,n,1
hs	0
en	0   line end is etx
kc	1
kr	0
cu	1	underscore cursor

}


end.
