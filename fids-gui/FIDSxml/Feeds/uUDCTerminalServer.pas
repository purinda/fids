unit uUDCTerminalServer;

{	terminal server and UDC management/polling extension of cUDCPkt }

interface

uses
	uUDC;

type
	cUDCTerminalServer = class( cUDCPkt )
    	public
            // procedure   PollUDP( const ip : string; port : int );  // not supported by UDC4
			procedure   BroadcastHost( port : int; hr, min : byte );  // invite tcp connections to port
			procedure   SetHost( const ip : string; port : int );  reintroduce;// invite tcp connection from ip to myIP:port
			procedure   SetSerialPort( const ip : string; port : int );
			procedure   SendSerialPort( const ip : string; const id : int; const str : string );
    	end;


implementation

uses
	SysUtils, ASCII, IdGlobal;



procedure   cUDCTerminalServer.BroadcastHost( port : int; hr, min : byte );  // under development

	var
    	i : int;
        //IpVal : cardinal;
        //old : boolean;
	begin
	{$ifdef LogUDC }  Log( 'BroadcastHost port=' + IntToStr( port ) );  {$endif }
    //old := mBroadcast;
    //SetDestination( ip );  //  mBroadcast := false;  // to ip
	//NewPkt;
    //mUDP.Host := ip;  // eg '192.168.0.20'
    for i := 1 to 1 do  begin   // was 2  resend to work around windows ARP bug - apparently not
        SetBroadcast; // ( old );  // mBroadcast := true;
        SetFrame( 0 );  // very broadcast

        {GlobalObjectID( oDisplay, 0 );
        Add( aSetHosts );   // don't del with other actions
        Add( 1 );  // N single host
        Add( 0 );  // channel ?
        Add( 0 );  // id ?
        IpVal := IPv4ToDWord( mLocalIP );  // todo try
        Add( ( IpVal shr 24 ) and $FF );  // IpVal := IpVal shr 8;  // byte order ?
        Add( ( IpVal shr 16 ) and $FF );  // IpVal := IpVal shr 8;
        Add( ( IpVal shr 8 ) and $FF );  // IpVal := IpVal shr 8;
        Add( IpVal and $FF );
        AddWord( port ); }

        GlobalObjectID( oDisplay, 0 );

        //GlobalObjectID( oDisplay, 0 );    // must be separate action list
        Add( aTime );
        Add( hr );
        Add( min );

        Add( aEpoch );
        AddInt( mEpoch );  // boot id for udc - triggers log in behaviour ?

        Add( aSIP );
        try    AddInt( IPv4ToDWord( mLocalIP ) );
        except  end;  // swallow any exception
        AddWord( port );
        Send;
        Sleep( 10 );
	    end;
    end;
{TCP Connection from 192.168.0.20
TCP Data from 192.168.0.20      no data sent    just do disconnect
TCP Disconnect}



procedure   cUDCTerminalServer.SetHost( const ip : string; port : int );  // under development todo move to cUDCMgt one fine day

	var
    	i : int;
        //IpVal : cardinal;
        //old : boolean;
	begin
	Log( 'SetHost ip=' + ip + ' port=' + IntToStr( port ) );
    SetDestination( ip );  //  mBroadcast := false;  // to ip
    // SetFrame( 0 );  // very broadcast
    for i := 1 to 1 do  begin   // was 2  resend to work around windows ARP bug - apparently not
        GlobalObjectID( oDisplay, 0 );
        Add( aTime ); // dummy time needed to trigger UDC TCP client into host
        Add( 12 );   // hr
        Add( 0 );    // min
        Add( aEpoch );
        AddInt( mEpoch );  // boot id for udc - triggers log in behaviour ?
        Add( aSIP );
        try    AddInt( IPv4ToDWord( mLocalIP ) );
        except  end;  // swallow any exception
        AddWord( port );
        Send;
        Sleep( 10 );
    	end;
    SetBroadcast; //  mBroadcast := true;
    end;


procedure   cUDCTerminalServer.SetSerialPort( const ip : string; port : int );

	begin    // assumes host IP has already been set by SetHost above
	Log( 'SetSerialPort ip=' + ip + ' : ' + IntToStr( port ) );
    SetDestination( ip );

	GlobalObjectID( oDisplay, 0 );
    Add( aSetIODev );
    Add( 1 );  // type is micro terminal
    AddWord( port );
    // Add( 0 );  // no polling
    Add( 1 );  // one device to poll
    Add( 1 );  // id = 1
    Send;

    Sleep( 500 );        // still need delay in UDC v2.48.0[2.42.0]
	GlobalObjectID( oDisplay, 0 );  // MUST BE IN SEPARATE MESSAGE TO aSetIODev
    Add( aPutIODev );
    AddASCII( '01KB ' + ip + cr + etx );
    Send;
    SetBroadcast;
    end;
{COM3 4388.9  01<1B>p<03>   adr esc p etx  typical keyad polls with type=1
COM3 4389.3  01<1B>p<03>
COM3 4389.7  01<1B>p<03>

and response to typing '1234567890'<enter>
00 00 00 01 01 00 00 04 0C 30 31 31 32 33 34 35 36 37 38 39 30
COM3 7304.1  01<1B>p<03>01HELLO out there 00000000<0D><03>01<1B>p<03>
}


procedure   cUDCTerminalServer.SendSerialPort( const ip : string; const id : int; const str : string );

    var
        IdStr : string;
	begin
	Log( 'SendSerialPort ip=' + ip + ' : ' + IntToStr( id ) + ' "' + str + '"' );
    if ip <> '' then  SetDestination( ip )
    else  begin     // they can all have it
        SetFrame( 0 );
    	SetBroadcast;
    	end;
	GlobalObjectID( oDisplay, 0 );
    Add( aPutIODev );
    // AddASCII( '01HELLO out there 00000000' + cr + etx );
    if id > 0 then  begin     // format to suit keypad , add keypad id '01' and cr + etx
        if id < 10 then  IdStr := '0' + IntToStr( id ) + str + cr + etx
        else  IdStr := IntToStr( id ) + str + cr + etx;
        AddASCII( IdStr );
    	end
    else  AddASCII( str );
    Send;
    SetBroadcast;
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
