unit uUDC;

// (C) ALphasoft Pty Ltd

interface

uses
	SysUtils, Graphics, IdUDPClient, IdBaseComponent, IdComponent, IdUDPBase, IdGlobal,
	Types, Classes;

type
	int = integer;
	aUDCattributes = ( udcFile, udcBlink, udcHScroll, udcVScroll, udcWipe, udcDirection, udcSpeed2, udcUTF16,
    	udcRightToLeft, udcCenter, udcRight );
	sUDCattributes = set of aUDCattributes;
	aUDCAddress = record
		case boolean of
			false : ( FrameN, LineN, FieldN : byte );  // for dopey udc addresses
			true :  ( UDCaddress : int );
		end;

const
	MaxUDP = 1200;   // nominal size limit for UDP packets
	UDCBlinkColour = 5;
	SplitID = 250;   // must be less than 255
	MaxPage = 10;
	{$ifdef DEBUG } {$define LogUDC } {$endif }    // logging would soon fill the drive

type
	aStats = record
		PktCount : int;
		ByteCount : int;
		end;
	apStats = ^ aStats;

	// aHAlign = ( haLeft, haCenter, haRight );
	aVAlign = ( vaBase, vaCenter );
	aUDCPage = record
		Page : int;
		Frame : int;
		Seconds : int;
		ClearPage : boolean;
		end;
	apUDCPage = ^ aUDCPage;

    aUDCID = array [0..11] of byte;  // 0 is size.....

type
	cUDCPkt = class( TIdUDPClient )
			constructor Create( log : boolean );
			destructor  Destroy;   override;
		public
			mPktCount : int;
			mByteCount : int;
			procedure   SetFrame( frame : int; stats : apStats = nil );
		protected
			mBuffer : TBytes;  //array [ 0.. MaxBuffer ] of byte;  // array of byte;
			mX : int;
			mUDPFrame : int;
			mPage : int;  // not supported yet
			// mUDP : TIdUDPClient;
			mSizeIndex : int;
			mActionIndex : int;
			//mBaseLine, mTxtWidth : int;
			//mCanvas : TCanvas;
			mLastSend, mSendInterval : cardinal;  // ticks ( mSec ) at last send - UDP throttling control
			mLastSendSec : int;
			mStats : apStats;
			mPort : int;
            mLocalIP : string;  // = GStack.LocalAddresses[ 0 ];
    		mEpoch: int;
			mBroadcast : boolean;  // => frame address
			mInit : boolean;
			procedure   Log( const s : string );
			procedure   Add( b : byte );
			procedure   AddWord( w : word );
			procedure   Add4Words( const r : TRect );
			procedure   AddInt( n : int );
			procedure   AddASCII( const name : string );
			procedure   AddStr( const name : string );
			procedure   AddUTF8Str( str : Utf8String );
			procedure   AddID( const id : aUDCID );
			procedure   Send;

            procedure   SetBroadcast;
			procedure   SetDestination( const ip : string );

			procedure   NewPkt;
			procedure   ActionsStart;
			procedure   ActionsEnd( check : boolean = true );
			//procedure   AddID( id1 : byte = 0;  id2 : byte = 0; id3 : byte = 0 );
			procedure   MakeObjID( id : aUDCAddress; var UdcId : aUDCID );
			procedure   GlobalObjectID( obj : byte; id : int );
			function    ObjectID( obj : byte; id : aUDCAddress; sub : int = -1 ) : aUDCID;
			//procedure   ObjectIDn( obj : byte; id : aUDCAddress; sub : int );  // vscroll items only
			procedure   ObjectIDfi( obj : byte; id : aUDCAddress; frame, item : int );  // vscroll items only
			//procedure   ObjectIDn0( obj : byte; id : aUDCAddress; sub : int );  // vscroll only
        private
			mLog : textfile;    // 'UDC.log'
			mDoLog : boolean;
    	end;



	cUDC = class( cUDCPkt )
		public
			constructor Create;
            destructor  Destroy;  override;
			//procedure   Blink( cl : TColor );
			procedure   TfrFile( name : string );
			procedure   SetGIF( name : string; id : int );
			procedure   SetFont( name : string; id : int );
			procedure   PutText( const txt : string; attrib : sUDCattributes );
			procedure   PutAttribute( font, size : int;  FGColour : TColor;  attrib : sUDCattributes ); // hal : aHAlign );
			procedure   TextObject( id : aUDCAddress; x, y, n : int;  TextStyle : int = 0 );
			function    MakeUDCWindow( id : aUDCAddress; const r : Trect; bg : TColor ) : aUDCID;

			procedure   ImageOut( const name : string;  id : aUDCAddress; const r : TRect );
			//procedure   WrStr( s : string );
			procedure   DefineTextStyle( id, font, size : int;  fg : TColor;  attrib : sUDCattributes ); // , bg ... hal : aHAlign; val : aVAlign );
            procedure   VScrollImage( id : aUDCAddress; const r : TRect; item : string; frame, itm : int );
            procedure   VScrollText( id : aUDCAddress; x, y, TextStyle : int;
            				item : string; frame, itm : int;  attrib : sUDCattributes );
			procedure   VScrollText1( id : aUDCAddress; x, y : int; frame, itm : int );
			procedure   VScroll( id : aUDCAddress; const r : TRect; bg : TColor;
							nitems : int; atrib : sUDCattributes );
			procedure   ObjOut( id : aUDCAddress; const r : TRect; bg : TColor; x, y, TextStyle : int;
							s : string; atrib : sUDCattributes = []; hsWidth : int = 0 );
			//procedure   TextOut( id : aUDCAddress; const r : TRect; bg : TColor; x, y, TextStyle : int;
							//s : string; atrib : sUDCattributes = []; hsWidth : int = 0 );
			procedure   TextTempFont( id : aUDCAddress; x, y, n, font, size : int; FGColour : TColor;
							txt : string; attrib : sUDCattributes ); //  hal : aHAlign;
			procedure   ClearScreen( page : int; col : TColor; name : string = '' );
			procedure   Update;
			procedure   Show;
			//function    Connection : boolean;
			procedure   SetPageMap( const ip : string; const pages : TList { of apUDCPage } );
			procedure   Init( const ip : string; frame : int; interval : int; pages : TList );
		end;

const

// Object Types
	oDisplay = 0;
	oPage = 1;
	oTextStyle = 2;
	oFile = 3;
	oFont = 4;
	oWindow = 5;
	oText = 6;
	oUtext = 7;
	oTable = 8;
	oLine = 9;
	oRect = 10;
	oImage = 11;
	oHScroll = 12;
	oVScroll = 13;
	oFFile = 14;
	oGroup = 15;
    oUTFtext = 16;
    oClock = 17;

// Actions
	aNew = 0;
	aDel = 1;
	aPut = 2;
	aShow = 3;
	aSetMap = 4;
	aSetBlink = 5;
	aPutAttr = 6;
	aDelBlink = 7;
	aClrr = 8;
	aClrc = 9;
	aDelr = 10;
	aPutr = 11;
	aAltcol = 12;
	aPutf = 13;
	aFrames = 14;
	aSip = 15;
	aTime = 16;
	aReset = 17;
	aReboot = 18;
	aGroup = 19;
	aEpoch = 20;
	aCarousel = 21;
	aDump = 22;
	aDebug = 23;
	aName = 24;
	aPoll = 25;
	aUpdate = 26;
	aFlash = 27;
	aVsRate = 28;
	aHsRate = 29;
	aCondef = 30;
	aSetChan = 31;
	aSetHosts = 32;
	aSetIODev = 33;
	aPutIODev = 34;
	aSetGateway = 35;
	aPutm = 36;
	aBlank = 37;
	aOffset = 38;



function ColorToPaletteNumber ( Colour: TColor ): byte;
function PaletteNumberToColor ( Colour: byte ): TColor;

implementation

uses
	Windows, Dialogs, uGlobal, uPoller, IdStack, ASCII;


const
	IDbroadcast = 0;  // broadcast
    UdcPort = 9200;
	UDCFrame0 = 1000;  // first 0..999 are group addresses
	UDCHeaderSize = 5;
	udcBlack = $FF;


var
{$IFDEF Palette64}
	PaletteTable: array [0..3] of byte = ($00, 68, $C0, $FF);   // Build Palette
	//PaletteTable: array [0..3] of byte = ($00, $80, $C0, $FF);   // Build Palette
	FlashPalette: array [0..3] of byte = ($40, $41, $42, $43);
{$ELSE}
	FlashPalette: array [0..3] of byte = (5, 151, 173, 193);
{$ENDIF}


{$IFDEF Palette64}

function ColourByteToColourBits (Colour: byte): byte;
	begin
		if Colour < $40 then
			Result := 0
		else if Colour < $A0 then
			Result := 1
		else if Colour < $E0 then
			Result := 2
		else
			Result := 3;
	end;

function ColorToPaletteNumber (Colour: TColor): byte;
	var
		i: byte;
	begin
		Result := $00;
		for i := 1 to 3 do
			begin
				Result := (Result shl 2) + ColourByteToColourBits (Colour shr 16);
				Colour := Colour shl 8;
			end;
	end;

function PaletteNumberToColor (Colour: byte): TColor;
	var
		i: byte;
	begin
		Result := 0;
		for i := 1 to 3 do
			begin
				Result := (Result shl 8) + PaletteTable [(Colour shr 4) and $03];
				Colour := Colour shl 2;
			end;

	end;

{$ELSE}

const

	UDCPalette: array [byte] of TColor =
		(
			$FFFFFF, $AAFFFF, $55FFFF, $00FFFF, $FFAAFF, $AAAAFF, $55AAFF, $00AAFF,
			$FF55FF, $AA55FF, $5555FF, $0055FF, $FF00FF, $AA00FF, $5500FF, $0000FF,
			$0000EE, $EEEEEE, $9EEEEE, $4FEEEE, $00EEEE, $EE9EEE, $9E9EEE, $4F9EEE,
			$009EEE, $EE4FEE, $9E4FEE, $4F4FEE, $004FEE, $EE00EE, $9E00EE, $4F00EE,
			$4900DD, $0000DD, $DDDDDD, $93DDDD, $49DDDD, $00DDDD, $DD93DD, $9393DD,
			$4993DD, $0093DD, $DD49DD, $9349DD, $4949DD, $0049DD, $DD00DD, $9300DD,
			$8800CC, $4400CC, $0000CC, $CCCCCC, $88CCCC, $44CCCC, $00CCCC, $CC88CC,
			$8888CC, $4488CC, $0088CC, $CC44CC, $8844CC, $4444CC, $0044CC, $CC00CC,
			$AAFFAA, $55FFAA, $00FFAA, $FFAAAA, $BBBBBB, $5DBBBB, $00BBBB, $FF55AA,
			$BB5DBB, $5D5DBB, $005DBB, $FF00AA, $BB00BB, $5D00BB, $0000BB, $FFFFAA,
			$EEEE9E, $9EEE9E, $4FEE9E, $00EE9E, $EE9E9E, $AAAAAA, $55AAAA, $00AAAA,
			$EE4F9E, $AA55AA, $5555AA, $0055AA, $EE009E, $AA00AA, $5500AA, $0000AA,
			$000099, $DDDD93, $93DD93, $49DD93, $00DD93, $DD9393, $999999, $4C9999,
			$009999, $DD4993, $994C99, $4C4C99, $004C99, $DD0093, $990099, $4C0099,
			$440088, $000088, $CCCC88, $88CC88, $44CC88, $00CC88, $CC8888, $888888,
			$444488, $004488, $CC0088, $880088, $55FF55, $00FF55, $FFAA55, $BBBB5D,
			$5DBB5D, $5DBB5D, $00BB5D, $00BB5D, $FF5555, $FF5555, $BB5D5D, $BB5D5D,
			$777777, $007777, $FF0055, $BB005D, $770077, $000077, $FFFF55, $AAFF55,
			$9EEE4F, $4FEE4F, $00EE4F, $EE9E4F, $AAAA55, $55AA55, $00AA55, $EE4F4F,
			$AA5555, $666666, $006666, $EE004F, $AA0055, $660066, $000066, $EEEE4F,
			$DDDD49, $93DD49, $49DD49, $00DD49, $DD9349, $99994C, $4C994C, $00994C,
			$DD4949, $994C4C, $555555, $005555, $DD0049, $99004C, $550055, $000055,
			$000044, $CCCC44, $88CC44, $44CC44, $00CC44, $CC8844, $888844, $448844,
			$008844, $CC4444, $884444, $444444, $004444, $CC0044, $880044, $440044,
			$00FF00, $FFAA00, $BBBB00, $5DBB00, $00BB00, $FF5500, $BB5D00, $777700,
			$007700, $FF0000, $BB0000, $770000, $333333, $FFFF00, $AAFF00, $55FF00,
			$4FEE00, $00EE00, $EE9E00, $AAAA00, $55AA00, $00AA00, $EE4F00, $AA5500,
			$666600, $006600, $EE0000, $AA0000, $660000, $222222, $EEEE00, $9EEE00,
			$93DD00, $49DD00, $00DD00, $DD9300, $999900, $4C9900, $009900, $DD4900,
			$994C00, $555500, $005500, $DD0000, $990000, $550000, $111111, $DDDD00,
			$CCCC00, $88CC00, $44CC00, $00CC00, $CC8800, $888800, $448800, $008800,
			$CC4400, $884400, $444400, $004400, $CC0000, $880000, $440000, $000000
		);

	VDGPalette: array [0..15] of TColor =
		(
			$000000,   //0 Black
			$008000,   //1 Green
			$3020B8,   //2 Red
			$00A0B0,   //3 Yellow
			$C00000,   //4 Blue
			$808000,   //5 Cyan
			$A000A0,   //6 Purple
			$FFFFFF,   //7 White
			$404040,   //8 Gray
			$580058,   //9 dim Purple
			$383800,   //A dim Cyan
			$600000,   //B dim Blue
			$005070,   //C dim Yellow
			$000090,   //D dim Red
			$005000,   //E dim Green
			$0068D0    //F Orange
		);

function ColourReservedFlash ( Col: byte ): boolean;
	var
		i: byte;
	begin
	Result := false;
	i := 0;
	repeat
		if i > High (FlashPalette) then        break;
		if Col = FlashPalette [i] then         begin
			Result := true;
			break;
			end;
		Inc (i);
	until false;
	end;


function ColourError (Col1, Col2: TColor): integer;
	begin
		Result := 0;
		repeat
			Result := Result + abs ((Col1 and $0000FF) - (Col2 and $0000FF));
			Col1 := Col1 shr 8;
			Col2 := Col2 shr 8;
		until Col1 = Col2;
	end;


function ColorToPaletteNumber ( Colour: TColor ): byte;

	var
		Col:  int;
		Best: integer;
		Err:  integer;
	begin
	Result := 0;
	Best := maxint;
	Col := $00;
	repeat
		if not ColourReservedFlash( Col ) then  begin
			Err := ColourError( Colour, UDCPalette [ Col ] );
			if Err < Best then  begin
				Best := Err;
				Result := Col;
				end;
			end;
		Inc( Col );
		until Col = $100;
	end;


function PaletteNumberToColor ( Colour: byte): TColor;

	begin
	// Result := $000000;
	{if Colour < Length (UDCPalette) then }   Result := UDCPalette [Colour];
	end;

{$ENDIF}

// __________________________ cUDCPkt __________________________________________

constructor cUDCPkt.Create( log : boolean );

	begin
    inherited Create;
    mPort := UdcPort;
    Port := UdcPort;
	mLocalIP := GStack.LocalAddresses[ 0 ];
    mDoLog := log;
    SetLength( mBuffer, MaxUDP + 300 );  // set decent initial buffer size
	if log then  begin
    	System.Assign( mLog, 'UDC.log' );
        try
        	Rewrite( mLog );
        except  end;
        end;
    mEpoch := Trunc( Now() * 60 * 60 * 24 );
    end;


destructor  cUDCPkt.Destroy;

	begin
    if mDoLog then CloseFile( mLog );
    end;


procedure  cUDCPkt.NewPkt;    // build udc pky header

	begin
	Send;   // flush any existing stuff
	mBuffer[ mX ] := IDbroadcast;   //Add( IDbroadcast );  avoid circular call
	Inc( mX );
	if mBroadcast then  begin
    	if mUDPFrame <> 0 then  AddWord( mUDPFrame + UDCFrame0 )  // frame number addressing
        else                    AddWord( 0 );
	    end
	else                AddWord( 0 );
	mSizeIndex := mX;
	AddWord( 0 );  // size hole
	mActionIndex := 0;  // ie not applicable
	end;


procedure  cUDCPkt.Send;

	var
		x : int;
        t : cardinal;
		//s : string;
	begin
	if mX > 5 then  begin
		x := mX;
		SetLength( mBuffer, x );
		ActionsEnd( false );
		mX := mSizeIndex;   // write size word
		AddWord( x - UDCHeaderSize );
		t := GetTickCount;
		if t - mLastSend < mSendInterval then  begin
			Sleep( mSendInterval - t + mLastSend );  // no rush
			mLastSend := GetTickCount;
			end
		else  mLastSend := t;

		if mBroadcast then  Broadcast( mBuffer, mPort )
		else                SendBuffer( mBuffer );   // to IP address
		Inc( mPktCount );
		mByteCount := mByteCount + x;
		if mStats <> nil then  begin
			Inc( mStats.PktCount );
			Inc( mStats.ByteCount, x );
			end;
		//{$ifdef LogUDC }  Log( 'send' );  {$endif }
		// Sleep( 10 );  // 10 output throttling    todo  remove  see above
		end;
	mX := 0;   // ie pkt buffer empty and unitialised
	end;


procedure  cUDCPkt.SetBroadcast;

	begin
    if not mBroadcast then  begin
        Send;
        mBroadcast := true;
        NewPkt;
    	end;
    end;


procedure  cUDCPkt.SetDestination( const ip : string );

	begin
    Send;
    mBroadcast := false;
	Host := ip;
    NewPkt;
    end;


procedure  cUDCPkt.SetFrame( frame : int; stats : apStats  );

	begin
	// if frame <> mUDPFrame then  begin
		Send;  // ie flush any existing to original frame address and new to new frame
		mStats := stats;
		mUDPFrame := frame;
		{$ifdef LogUDC }
		Log( '' );
		Log( 'SET FRAME = ' + IntToStr( frame ) + '   sec ' + IntToStr( Seconds ) );
		{$endif }
		// end;
	end;


procedure  cUDCPkt.Add( b : byte );

	begin
	if mX = 0 then  NewPkt;
	if mX > High( mBuffer ) then  SetLength( mBuffer, mX * 2 );  // grow buffer
	mBuffer[ mX ] := b;         // High returns -1 for empty, max index once defined
	Inc( mX );
	end;


procedure  cUDCPkt.AddWord( w : word );

	begin
	Add( Hi( w ) );    // big end first
	Add( Lo( w ) );
	end;


procedure  cUDCPkt.Add4Words( const r : TRect );

	begin
	AddWord( r.Left );  AddWord( r.Top );    // x, y
	AddWord( r.Right - r.Left + 1 );  AddWord( r.Bottom - r.Top + 1 );    // w, h
	end;


procedure   cUDCPkt.AddInt( n : int );

	begin
	AddWord( n shr 16 );
	AddWord( n and $FFFF );
	end;


procedure   cUDCPkt.AddASCII( const name : string );

	var
		x, l : int;
	begin
	l := Length( name );
	if l <= 255 then  begin     // todo ? truncate
		Add( l );
		for x := 1 to l do  begin
			Add( Byte( Ord( name[ x ] ) and $00FF ) );
			end;
		end
	else  AddASCII( '???' );
	end;


procedure   cUDCPkt.AddStr( const name : string ); // for oUTFtext - otherwise put size in chars

	var
		x, l : int;
	begin
	l := ByteLength( name ); // * SizeOf( Char );     $$$
    // l := Length( name );
	if l <= 255 then  begin     // todo ? truncate
		Add( l );
		for x := 1 to Length( name ) do  begin
			AddWord( Ord( name[ x ] ) );
			end;
		end
	else  AddStr( '???' );
	end;


procedure   cUDCPkt.AddUTF8Str( str : Utf8String );

	var
		x, l : int;
	begin
	l := Length( str );
	if l <= 255 then  begin     // todo ? truncate
		Add( l );
		for x := 1 to l do  begin
			Add( Byte( str[ x ] ) );
			end;
		end
	else  AddUTF8Str( '???' );
	end;


procedure   cUDCPkt.ActionsStart;

	begin
	mActionIndex := mX;
	AddWord( 0 );
	end;


procedure   cUDCPkt.ActionsEnd( check : boolean = true );

	var
		x : int;
	begin            // only interested in page and page.field IDs
	if mActionIndex <> 0 then  begin
		x := mX;
		mX := mActionIndex;
		AddWord( x - mActionIndex - 2 );
		mActionIndex := 0;
		mX := x;
		if check and ( mX > MaxUDP ) then  begin
			Send;
			end;
		end;
	end;


procedure   cUDCPkt.GlobalObjectID( obj : byte; id : int );

	begin
	ActionsEnd;      // finish off earlier action list
	Add( obj );
    if id = 0 then    Add( 0 )
	else  begin  Add( 1 );  Add( id );  end;
	ActionsStart;
	end;


procedure  AddToID( var id : aUDCID; n : byte );

	begin
    Inc( id[ 0 ] );
    id[ id[ 0 ] ] := n;
    end;


procedure   cUDCPkt.AddID( const id : aUDCID );

    var
    	i : int;
	begin
    for i := 0 to id[ 0 ] do  Add( id[ i ] );
    end;


procedure   cUDCPkt.MakeObjID( id : aUDCAddress; var UdcId : aUDCID );

	begin
    UdcId[ 0 ] := 0;         // build id
    AddToID( UdcId, mPage );
	if id.FrameN <> 0 then  AddToID( UdcId, id.FrameN ); // n := 1
	if id.LineN <> 0 then   AddToID( UdcId, id.LineN ); // n := 2
	if id.FieldN <> 0 then  AddToID( UdcId, id.FieldN ); // n := 3
    end;


function   cUDCPkt.ObjectID( obj : byte; id : aUDCAddress; sub : int = -1 ) : aUDCID;

	begin
	ActionsEnd;      // finish off earlier action list
	Add( obj );
    MakeObjID( id, result );
	if sub >= 0 then  AddToID( result, sub );
	AddID( result );
	ActionsStart;
	end;


procedure   cUDCPkt.ObjectIDfi( obj : byte; id : aUDCAddress; frame, item : int );  // vscroll items only

    var
        UdcId : aUDCID;
	begin            //  6 | page | frame | line | scroll | frame | item   vscroll id
	ActionsEnd;      // finish off earlier action list
	Add( obj );
    MakeObjID( id, UdcId );
    AddToID( UdcId, frame );
    AddToID( UdcId, item );
	AddID( UdcId );
	ActionsStart;
	end;


procedure   cUDCPkt.Log( const s : string );

	begin
    if mDoLog then  Writeln( mLog, s );
	end;



// ____________________ cUDC ___________________________________________________


constructor cUDC.Create;

	begin
    inherited Create( true );   // log UDC as display
	SetLength( mBuffer, MaxUDP );
	// mPort := UdcPort;
	//mUDP := TIdUDPClient.Create;
	// mLocalIP := GStack.LocalAddresses[ 0 ];
	//mUDP.Host := '192.168.0.20';   // default   todo undefine here ?
	//mUDP.Port := mPort;
    mLastSend := GetTickCount;
	end;


destructor  cUDC.Destroy;

	begin
    // mUDP.Free;
    inherited;
	end;


function  FileOpenWin32( name : string ) : THandle;

	const
		GENERIC_WRITE =   $40000000;  // WinNT.h  CreateFile params
		GENERIC_READ  =   $80000000;
		FILE_SHARE_READ	= $00000001;  // WinNT.h
		OPEN_EXISTING   = 3;          // WinBase.h

	begin
	Result := CreateFile( PChar( name ), GENERIC_READ,
						FILE_SHARE_READ, Nil, OPEN_EXISTING, 0, 0);
	end;


procedure   cUDC.TfrFile( name : string );

	const
		chunk = MaxUDP - 100;    // 1400 the Graham Gallagher number
	var
		f : int;
		seq, sx, n : int;
	begin
	f := FileOpenWin32( name );
	if f > 0 then  begin
		{$ifdef LogUDC }  Log( 'TfrFile ' + name );  {$endif }
		n := FileSeek( f, 0, 2 );    // file size
		FileSeek( f, 0, 0 );

		NewPkt;
		GlobalObjectID( oFile, 0 );
		Add( aNew );
		AddASCII( LowerCase( ExtractFileName( name ) ) );    // todo full path ? NFG too long ??
		AddInt( n );
		Send;

		//file + ID(0) + ARGS(def + S(fname) + L(stat(f).size
		seq := 0;               // force buffer to be big enough
		if High( mBuffer ) < chunk + 90 then  SetLength( mBuffer, chunk + 100 );
		repeat
			NewPkt;
			GlobalObjectID( oFile, 0 );
			Add( aPut );
			AddWord( seq );  Inc( seq );
			sx := mX;
			AddWord( 0 );        // hole for size
			n := FileRead( f, mBuffer[ mX ], chunk );   // read straight into buffer
			if n > 0 then  begin
				mX := sx;          // retrospectively fix size word
				AddWord( n );
				mX := mX + n;
				Send;
				end;
			until n < chunk;  // eof
			// file + ID(0) + ARGS(put + W(seq++) + W(n) + data
		end;
	end;


procedure   cUDC.SetFont( name : string; id : int );

	begin
	// font + ID(1) + ARGS(def + S("times.ttf")
	GlobalObjectID( oFont, id );
	Add( aNew );
	AddASCII( LowerCase( ExtractFileName( name ) ) );      // todo full path ?
	{$ifdef LogUDC }  Log( 'SetFont ' + name + ' id=' + IntToStr( id ) );  {$endif }
	end;


procedure   cUDC.SetGIF( name : string; id : int );

	begin
	GlobalObjectID( oImage, id );
	Add( aPut );    // $$$ aNew ?? fix manual ??
	AddASCII( LowerCase( ExtractFileName( name ) ) );      // todo full path ?  NFG
	{$ifdef LogUDC }  Log( 'SetGIF ' + name + ' id=' + IntToStr( id ) );  {$endif }
	end;


procedure   cUDC.Show;

	begin
	{$ifdef LogUDC }  Log( 'Show' );  {$endif }
	GlobalObjectID( oPage, mPage );
	Add( aShow );
	Send;
	end;


procedure   cUDC.Update;

	begin
	{$ifdef LogUDC }  Log( 'Update' );  {$endif }
	GlobalObjectID( oPage, mPage );
	Add( aUpdate );
	Send;
	end;


procedure   cUDC.VScrollImage( id : aUDCAddress; const r : TRect; item : string; frame, itm : int );

	begin     // assumes oVScroll already created by calling VScroll
    ObjectIDfi( oImage, id, frame, itm );
    Add( aNew );
    Add4Words( r );
    Add( aPut );
    AddASCII( LowerCase( ExtractFileName( item ) ) );      // todo full path max 25 chars
    {$ifdef LogUDC }  Log( 'VScrollSubImage [' + IntToStr( frame ) + ', ' + IntToStr( itm ) + ']  ' + item );  {$endif }
	end;


procedure   cUDC.VScrollText( id : aUDCAddress; x, y, TextStyle : int;
							item : string; frame, itm : int;  attrib : sUDCattributes );

	begin     // assumes oVScroll already created by calling VScroll
    ObjectIDfi( oUTFtext, id, frame, itm );// ObjectIDfi( oUtext, id, frame, itm );
    Add( aNew );
    AddWord( x );  AddWord( y );
    Add( 1 );  Add( TextStyle );  	// size=1; text style
    PutText( item, attrib );
    //Add( aPut );
    //AddStr( item );
    {$ifdef LogUDC }  Log( 'VScrollSubText [' + IntToStr( frame ) + ', ' + IntToStr( itm ) + ']  ' + item );  {$endif }
	end;


procedure   cUDC.VScrollText1( id : aUDCAddress; x, y : int; frame, itm : int );

	begin     // assumes oVScroll already created by calling VScroll
    ObjectIDfi( oUTFtext, id, frame, itm );// ObjectIDfi( oUtext, id, frame, itm );
    Add( aNew );
    AddWord( x );  AddWord( y );
    Add( 0 );
    {$ifdef LogUDC }  Log( 'VScrollText1 [' + IntToStr( frame ) + ', ' + IntToStr( itm ) + ']  ' );  {$endif }
	end;


procedure   cUDC.VScroll( id : aUDCAddress; const r : TRect; bg : TColor;
							nitems : int; atrib : sUDCattributes );

	var
		style : int;
	begin
	ObjectID( oVScroll, id );
	Add( aNew );
	Add4Words( r );
	Add( nitems );
	Add( ColorToPaletteNumber( bg ) );
	style := 0;
	if udcWipe in atrib then   style := style or 8;
	if udcDirection in atrib then   style := style or 1;
    if udcSpeed2 in atrib then   style := style or $80;   // bit7 is speed ?
	Add( style );
	//Add( aPut );   // $$$ sus
	{$ifdef LogUDC }  Log( 'VScroll ' + IntToStr( nitems ) );  {$endif }
	end;


procedure   cUDC.ImageOut( const name : string;  id : aUDCAddress; const r : TRect );

	begin   // image + ID("1.1.1") + ARGS(def + Pt(30, 20) + DSZ + put + S("fruit.gif")
	ObjectID( oImage, id );
	Add( aNew );
	Add4Words( r );
	Add( aPut );
	AddASCII( LowerCase( ExtractFileName( name ) ) );      // todo full path ?
	//                      ActionsEnd;
	{$ifdef LogUDC }
	Log( 'ImageOut ' + name + ' at ' + IntToStr( r.Left ) + ',' + IntToStr( r.Top ) );
	{$endif }
	end;


function  HalignByte( attrib : sUDCattributes ) : byte;

	begin
    result := 0;
    if  udcCenter in attrib then  result := 1
    else if udcRight in attrib then  result := 2;
    end;


// all text styles use transparent background mode ( bg = black ) and no Vert align - done by PC. conserves text styles

procedure   cUDC.DefineTextStyle( id, font, size : int; fg : TColor; attrib : sUDCattributes ); //, bg ...hal : aHAlign; val : aVAlign );

	begin
	GlobalObjectID( oTextStyle, id );
	Add( aNew );
	Add( 1 );  Add( font );  // AddID( font );    // must be id not Add( byte )
	Add( size );  // font size
	Add( ColorToPaletteNumber( fg ) );
	Add( $FF );  // Add( ColorToPaletteNumber( bg ) );
	Add( 0 );  Add( 0 );
    Add( HalignByte( attrib ) );
    Add( 0 );  // all base line aligned.  Add( Byte( val ) );   centering etc done by PC
	{$ifdef LogUDC }  Log( 'DefineTextStyle id=' + IntToStr( id ) + ' font=' + IntToStr( font ) +
							' size=' + IntToStr( size ) );  {$endif }
	end;


function   cUDC.MakeUDCWindow( id : aUDCAddress; const r : Trect; bg : TColor ) : aUDCID;

	begin
	result := ObjectID( oWindow, id );   // window is child of page or line is child of window
	Add( aNew );
	// AddWord( r.Left - ref.X );  AddWord( r.Top - ref.Y );    // x, y
	AddWord( r.Left );  AddWord( r.Top );    // x, y
	AddWord( r.Right - r.Left + 1 );  AddWord( r.Bottom - r.Top + 1 );    // w, h
	Add( ColorToPaletteNumber( bg ) );  // bg
	Add( 0 );  { flat }    Add( 0 );    Add( 0 );
	// Add( aPut );    WRONG  $$$
	{$ifdef LogUDC }
	Log( 'MakeUDCWindow at ' + IntToStr( r.Left ) + ',' + IntToStr( r.Top ) );
	{$endif }
	end;


procedure   cUDC.ObjOut( id : aUDCAddress; const r : TRect; bg : TColor; x, y, TextStyle : int;
							s : string; atrib : sUDCattributes = []; hsWidth : int = 0 );
	begin
    if udcFile in atrib then  begin
        ImageOut( s, id, r );
        end
	else begin
        if udcHScroll in atrib then  begin
            ObjectID( oHScroll, id );   // window is child of line ( to avoid udc byte limit )
            Add( aNew );
            AddWord( r.Left );  AddWord( r.Top );    // x, y
            AddWord( r.Right - r.Left + 1 );  AddWord( r.Bottom - r.Top + 1 );    // w, h
            AddWord( hsWidth );
            Add( ColorToPaletteNumber( bg ) );
            Add( 4 );  // right to left  ( 1, 2, 3, 4, 7, 15 does the same thing exactly )
            end
        else  begin
            ObjectID( oWindow, id );   // window is child of line ( to avoid udc byte limit )
            Add( aNew );
            AddWord( r.Left );  AddWord( r.Top );    // x, y
            AddWord( r.Right - r.Left + 1 );  AddWord( r.Bottom - r.Top + 1 );    // w, h
            if udcBlink in atrib then  begin
                Add( UDCBlinkColour );  // bg colour  only colour 5 blinks
                Add( 0 );   Add( 0 );    Add( 0 );

                GlobalObjectID( oPage, mPage ); //  );    why doesn't this work !!!!!
                Add( aSetMap );
                Add( UDCBlinkColour );               // only colour 5 blinks
                Add( bg and $FF );  Add( ( bg shr 8 ) and $FF );  Add( bg shr 16 );    // extract rgb from int

                GlobalObjectID( oPage, mPage ); // %%% mPage );
                Add( aSetBlink );
                Add( UDCBlinkColour );
                Add( 0 );  Add( 0 );  Add( 0 );    // this is ignored - flashes to 18% grey !?!!
                end
            else  begin    // finish off normal window def
                Add( ColorToPaletteNumber( bg ) );  // bg
                Add( 0 );   Add( 0 );    Add( 0 );
                end;
            end;

        if s <> '' then  begin   // write the text
        	TextObject( id, x, y, 1, TextStyle );
            PutText( s, atrib );

            {ObjectID( oUtext, id, 1 );  // text is child of window
            Add( aNew );
            AddWord( x );  AddWord( y );	// x, y
            Add( 1 );  Add( TextStyle );  	// size=1; text style as id
            Add( aPut );
            AddStr( s ); }
            end;
        end;
	// Send;
	// Show;
	{$ifdef LogUDC }  Log( 'ObjOut ''' + s + ''' style=' + IntToStr( TextStyle ) +
			' at ' + IntToStr( r.Left ) + ',' + IntToStr( r.Top ) );  {$endif }
	end;

{procedure   cUDC.TextOut( id : aUDCAddress; const r : TRect; bg : TColor; x, y, TextStyle : int;
							s : string; atrib : sUDCattributes = []; hsWidth : int = 0 );   // obs

	begin
	if udcHScroll in atrib then  begin
		ObjectID( oHScroll, id );   // window is child of line ( to avoid udc byte limit )
		Add( aNew );
		AddWord( r.Left );  AddWord( r.Top );    // x, y
		AddWord( r.Right - r.Left + 1 );  AddWord( r.Bottom - r.Top + 1 );    // w, h
		AddWord( hsWidth );
		Add( ColorToPaletteNumber( bg ) );
		Add( 4 );  // right to left  ( 1, 2, 3, 4, 7, 15 does the same thing exactly )
		// Add( aPut );    $$$ WRONG ???
		end
	else  begin
		ObjectID( oWindow, id );   // window is child of line ( to avoid udc byte limit )
		Add( aNew );
		AddWord( r.Left );  AddWord( r.Top );    // x, y
		AddWord( r.Right - r.Left + 1 );  AddWord( r.Bottom - r.Top + 1 );    // w, h
		if udcBlink in atrib then  begin
			Add( UDCBlinkColour );  // bg colour  only colour 5 blinks
			Add( 0 );   Add( 0 );    Add( 0 );
			// Add( aPut );     $$$ WRONG ???

			GlobalObjectID( oPage, mPage ); //  );    why doesn't this work !!!!!
			Add( aSetMap );
			Add( UDCBlinkColour );               // only colour 5 blinks
			Add( bg and $FF );  Add( ( bg shr 8 ) and $FF );  Add( bg shr 16 );    // extract rgb from int

			GlobalObjectID( oPage, mPage ); // %%% mPage );
			Add( aSetBlink );
			Add( UDCBlinkColour );
			Add( 0 );  Add( 0 );  Add( 0 );    // this is ignored - flashes to 18% grey !?!!
			end
		else  begin    // finish off normal window def
			Add( ColorToPaletteNumber( bg ) );  // bg
			Add( 0 );   Add( 0 );    Add( 0 );
			// Add( aPut );    $$$ WRONG ???
			end;
		end;

	if s <> '' then  begin   // write the text
		ObjectID( oUtext, id, 1 );  // text is child of window
		Add( aNew );
		AddWord( x );  AddWord( y );	// x, y
		Add( 1 );  Add( TextStyle );  	// size=1; text style as id
		Add( aPut );
		AddStr( s );
		end;
	// Send;
	// Show; }
	//{$ifdef LogUDC }  Log( 'TextOut ''' + s + ''' style=' + IntToStr( TextStyle ) +
			//' at ' + IntToStr( r.Left ) + ',' + IntToStr( r.Top ) );  {$endif }
	//end;


procedure   cUDC.PutText( const txt : string;  attrib : sUDCattributes );

	begin
    Add( aPut );
    Add( Ord( udcRightToLeft in attrib ) );   // ( left to right, right to left ) from normal start point
    if udcUTF16  in attrib then  begin
        Add( 1 );
        AddStr( txt );  // but size in bytes - usually words !! ( to quote FIDSmod2 "UDC is a strange box" )
        end
    else  begin
        Add( 0 );   // ( UTF8, UTF16 )
        AddUTF8Str( UTF8String( txt ) );   // was AddStr
    	end;
    end;


procedure   cUDC.PutAttribute( font, size : int;  FGColour : TColor;  attrib : sUDCattributes ); // hal : aHAlign );

	begin
    Add( aPutAttr );
    Add( 1 );  Add( font );
    Add( size );
    Add( ColorToPaletteNumber( FGColour ) );   Add( $FF );
    Add( 0 );   Add( 0 );
    // Add( Ord( hal ) );
    Add( HalignByte( attrib ) );   // 0 works
    Add( 0 );   // v align none
    end;


procedure   cUDC.TextObject( id : aUDCAddress; x, y, n : int;  TextStyle : int = 0 );

	begin
    ObjectID( oUTFtext, id, n ); // ObjectID( oUtext, id, n );  // text is child of window
    Add( aNew );
    AddWord( x );  AddWord( y );	// x, y
    if TextStyle = 0 then    Add( 0 )
	else  begin  Add( 1 );  Add( TextStyle );  end;
    end;


procedure   cUDC.TextTempFont( id : aUDCAddress; x, y, n, font, size : int; FGColour : TColor;
				txt : string; attrib : sUDCattributes );

	begin
    TextObject( id, x, y, n );
    PutAttribute( font, size, FGColour, attrib );
    PutText( txt, attrib );
    end;

// eg	WINDOW 0.1.3 New x=0 y=320 1360x288 | TEXTSTYLE 5 New 5 size=96 col=0/255 cn | WINDOW 0.1.3.1 New x=0 y=0 1360x288 |
//		UTEXT 0.1.3.1.1 New x=680 y=123 0 PutAttr 5 sz=115 Put "Sydney" | UTEXT 0.1.3.1.2 New x=680 y=244  5 Put "London Paris" |
// eg	id=0 fr=1001 len=120  TEXTSTYLE 5 New 5 size=96 col=0/255 cn | WINDOW 0.1.3.1 New x=0 y=0 1360x288 |
//		UTFTEXT 0.1.3.1.1 New x=680 y=123  0 PutAttr 5 sz=115 Put 0 0 "Brisbane" | UTEXT 0.1.3.1.2 New x=680 y=244  5 Put "London Paris" |


procedure   cUDC.ClearScreen( page : int; col : TColor; name : string = '' );

	begin
	{$ifdef LogUDC }  Log( 'ClearScreen page=' + IntToStr( page ) + ' ' + name );  {$endif }
	NewPkt;
	mPage := page;
	GlobalObjectID( oPage, page );
	Add( aDel );   // don't del with other actions
	// Send; // NewPkt;
	GlobalObjectID( oPage, page );
	Add( aNew );
	Add( ColorToPaletteNumber( col ) );
    if name <> '' then  begin
		Add( aName );
        AddASCII( name );
    	end;
	end;
(*
procedure   cUDC.PollUDP( const ip : string; port : int );

	//var
        //IpVal : cardinal;
	begin
	{$ifdef LogUDC }  Log( 'Poll ip=' + ip + ' : ' + IntToStr( port ) );  {$endif }
    if ip <> '' then  SetDestination( ip )
    else  begin     // they can all have it
        SetFrame( 0 );  /// $$$
    	SetBroadcast;
    	end;
	GlobalObjectID( oDisplay, 0 );
    Add( aPoll );
    try    AddInt( IPv4ToDWord( mLocalIP ) );
    except  end;  // swallow any exception
    {Add( ( IpVal shr 24 ) and $FF );  // bigendian    IpVal :=
    Add( ( IpVal shr 16 ) and $FF );  // IpVal := IpVal shr 8;
    Add( ( IpVal shr 8 ) and $FF );  // IpVal := IpVal shr 8;
    Add( IpVal and $FF );  }
    AddWord( port );
    Add( 13 );  // dummy
    Send;

    SetBroadcast;
    end;
{id=0 fr=0 len=12  DISPLAY 0 Poll 192.168.0.104 :9200 |
00 00 00 00 0C 00 00 00 08 19 C0 A8 00 68 23 F0 0D }
*)


procedure   cUDC.SetPageMap( const ip : string; const pages : TList { of apUDCPage } );

	var                       // probably won't work to frame addresses  todo test
		p, i : int;
		pp : apUDCPage;
		{$ifdef LogUDC } l : string; {$endif }
        //old : boolean;
	begin
	Send;
	{$ifdef LogUDC }
    Log( '' );
	l := '  ';
	for p := 0 to pages.Count - 1 do  begin  // define and init each page
		pp := pages[ p ];
		l := l + ' ' + IntToStr( pp.Page ) + ',' + IntToStr( pp.Frame ) + ',' + IntToStr( pp.Seconds );
		end;
	Log( 'SetPageMap IP=' + ip  + l );
	{$endif }

	// Send;
    //old := mBroadcast;
	//SetBroadcast; // ( false );  //  mBroadcast := false;
	//mUDP.Host := ip;  // eg '192.168.0.20'
    SetDestination( ip );

	for i := 1 to 10 do  begin    // to avoid UDC packet loss problem  2 isn't enough

	  {	for p := 0 to pages.Count - 1 do  begin  // define and init each page
			pp := pages[ p ];
			if ( pp.Page <> 0 ) and ( pp.ClearPage ) then  begin   // may be problematic - assumes page 0 already defined and written to
				GlobalObjectID( oPage, pp.Page );
				Add( aNew );
				Add( udcBlack );
				end;
			end;
		Send;     }
		// NewPkt;
		GlobalObjectID( oDisplay, 0 );
		Add( aFrames );
		Add( pages.Count );
		for p := 0 to pages.Count - 1 do  begin
			pp := pages[ p ];
			AddWord( pp.Frame + UDCFrame0 );
			Add( pp.Page );
			end;
		Add( aCarousel );
		Add( pages.Count );
		for p := 0 to pages.Count - 1 do  begin
			pp := pages[ p ];
			Add( pp.Page );
			Add( pp.Seconds );
			end;
		ActionsEnd;
		Send;
		end;  // for i

	for p := 0 to pages.Count - 1 do  begin
		pp := pages[ p ];
		pp.ClearPage := false;  // don't clear on each init
		end;

	SetBroadcast; // ( old );  // mBroadcast := true;
    mPage := 0;  // everything else is broadcast
	end;


procedure   cUDC.Init( const ip : string; frame : int; interval : int; pages : TList );

	var
		i, n : int;
        id : aUDCAddress;
	begin     // todo del all, set up frame address
	mSendInterval := interval;     n := 1;
	if ( not mInit ) or ( Seconds - mLastSendSec > 10 ) then  n := 4;   // warm up router ??

	if ip <> '' then  begin
		mLastSendSec := Seconds;
		{$ifdef LogUDC }  Log( '' );  Log( 'Init IP=' + ip + ' frame=' + IntToStr( frame ) );  {$endif }
		for i := 0 to n - 1 do  begin
			//mUDP.Host := ip;  // eg '192.168.0.20'
			mUDPFrame := frame;
			SetDestination( ip ); //SetBroadcast( false );
			if pages <> nil then  SetPageMap( ip, pages )   // and revert to broadcast

			else  begin  // don't overwrite slide show frame address
				NewPkt;
				GlobalObjectID( oDisplay, 0 );
				Add( aFrames );
				Add( 1 );  // only do single frame (mPage) addressing  todo multi page support
				AddWord( frame + UDCFrame0 );   // frame 1 page 0 mapped to page 1
				Add( 0 );

				GlobalObjectID( oDisplay, 0 );   // turn off carousel
				Add( aCarousel );
				Add( 0 );
				end;

			ClearScreen( 0, clBlue );       // todo clear all pages    clBlue

			mPage := 0;

            DefineTextStyle( 1, 0, 42, clWhite, [] );
            id.UDCaddress := 0;
            TextObject( id, 20, 60, 1, 1 );
            PutText( 'DII UDC frame ' + IntToStr( frame ), [] );

			//GlobalObjectID( oPage, mPage );   // debug only
			//Add( aShow );  // debug only
			GlobalObjectID( oDisplay, 0 );     // $$$ does nothing
			Add( aVsRate );
            AddWord( 2 );      // rate  does nothing
            AddWord( 420 );    // delay 420 is standard ( really slow )

			Send;

			SetBroadcast;  // ( true );  // mBroadcast := true;
   			mPage := 0;  // everything else is broadcast

			if not mInit then  begin    // warm up router ??
				mUDPFrame := 1;
				Show;               // debug shows frame number on first screen
				end;

			end;  // for
        mInit := true;
		end;
	end;


{ NOTES

UDC / ID conventions :-

TObject use    |page no|frame id|line id | field no | subfield    (to avoid byte limit)

Amazing things learned the hard way :-

1	don't redefine text style 0 - this is read only
2	UDC file name limit is 27 (25) chars - full path name doesn't work
3	files cannot have duplicaed names - eg large and small QF.GIF doesn't work - always get same one
4	oPage aDel must be in its own action list. ie follow with an object ID
5	oPage aNew must be done before any witing to page is accepted
6	fonts ( and files) must be declared (oFont..aNew) before referencing them in text style ( can download later )
7	need a few mSec delay between packets  10 mSec mostly works
8	max packet size is ~1400 bytes    ( 1200 seems better )
9	udc font 0 is slightly looser than windows ~ 5%   Arial bold
10	but cyberbit is tighter   adjustable ?
11	aVsRate    not adjustable in some versions < 2.48[2.42]
12	VScroll with nothing to scroll crashes the UDC
13	first UDP packets sent get lost - proly windows/router prob - apparently not
14	must multiple send SetPageMap  x10 and Init WTF
15  UDC TCP listens on port 80 only - not normal drawing port
16	aPutIODev MUST BE IN SEPARATE MESSAGE TO aSetIODev after a big time delay ~ 1 Second
17  time is required to get UDC to client in to Host IP

etc etc

typical time update :-
id=0 fr=1001 len=55  WINDOW 0.1.1.3  New x=1210 y=0  | UTEXT 0.1.1.3.1  New 2   Put "18:52"  | PAGE 0  Update |
00 03 E9 00 37 05 04 00 01 01 03 00 0D 00 04 BA 00 00 00 96 00 3C E9 00 00 00 07 05 00 01 01 03 01 00 13 00 00 4B 00 24 01 02 02 05 00 31 00 38
00 3A 00 35 00 32 01 01 00 00 01 1A

SET FRAME = 1   sec 1257
TextOut '18:52' style=2 at 1210,0
Update
send

UDC
	DISPLAY .id8 = 0
					:frames    frame addr / page id  pairs
					:carousel  slide show
					:group     group address
					:sip   host IP addr
					:time
					:vsrate  scroll rate
					:setchan  channel addr of UDC
					:sethosts
					:setiodev
					:blank

		PAGE  .id8
					.colour map = palette
					:show/update
					:dump bitmap
					:set blink  blinking alt colour
			TEXTSTYLE
					.id8
			FONT
					.id8
			WINDOW
					.id8
					.x y w h
					.colour
					.bevel

				TEXT/UTEXT   (8 bit/16 bit big end)
					.x y
					.text style id8
					.idN
				IMAGE
					.id8
					.x y w h
				HSCROLL / VSCROLL
					.x y h w
					.w2 colour dirn

wireshark samples :-    udc data starts at pkt[ $2A ]     first obj is pkt[ $2F ]

Init IP=192.168.0.20 frame=1          ip = c0.a8.0.14
ClearScreen page=0               x4
send
send
Show
send

0000  02 08 32 40 00 61 00 11  d8 b5 a2 a2 08 00 45 00   ..2@.a.. ......E.
0010  00 32 36 f2 00 00 80 11  82 62 c0 a8 00 02 c0 a8   .26..... .b......
0020  00 14 05 69 23 f0 00 1e  1b 01 00 00 00 00 11 00   ...i#... ........
0030  01 00 00 05 0e 01 03 e9  00 00 01 00 00 02 15 00   ........ ........

0000  02 08 32 40 00 61 00 11  d8 b5 a2 a2 08 00 45 00   ..2@.a.. ......E.
0010  00 6a 36 f3 00 00 80 11  82 29 c0 a8 00 02 c0 a8   .j6..... .)......
0020  00 14 05 69 23 f0 00 56  9d fd 00 00 00 00 49 01   ...i#..V ......I.
0030  01 00 00 01 01 01 01 00  00 02 00 c9 02 01 01 00   ........ ........
0040  0a 00 01 00 2a 00 00 00  00 00 00 07 02 00 01 00   ....*... ........
0050  27 00 00 14 00 3c 01 01  02 0f 00 44 00 49 00 49   '....<.. ...D.I.I
0060  00 20 00 55 00 44 00 43  00 20 00 66 00 72 00 61   . .U.D.C . .f.r.a
0070  00 6d 00 65 00 20 00 31                            .m.e. .1         

0000  ff ff ff ff ff ff 00 11  d8 b5 a2 a2 08 00 45 00   ........ ......E.
0010  00 27 36 f4 00 00 80 11  43 28 c0 a8 00 02 ff ff   .'6..... C(......
0020  ff ff 05 69 23 f0 00 13  23 bf 00 03 e8 00 06 01   ...i#... #.......
0030  01 00 00 01 03                                     .....   

SET FRAME = 1   sec 1
ClearScreen page=0
MakeUDCWindow at 0,0
MakeUDCWindow at 0,0
SetGIF \graphics\gifs\airlines\1360x160\t71360x160.gif id=1
WrImage T71360x160.gif at 0,0
MakeUDCWindow at 0,160
SetFont swissbc.ttf id=5
DefineTextStyle id=1 font=5 size=130
TextOut 'T7 899' style=1 at 0,0
DefineTextStyle id=2 font=5 size=40
TextOut 'Departure Time:' style=2 at 650,35
DefineTextStyle id=3 font=5 size=40
TextOut '0008' style=3 at 1150,35
TextOut 'Boarding at Gate:' style=2 at 650,95
DefineTextStyle id=4 font=5 size=40
TextOut '1' style=4 at 1150,95
MakeUDCWindow at 0,310
DefineTextStyle id=5 font=5 size=100
TextOut 'Adelaide Chiclayo' style=5 at 0,0
MakeUDCWindow at 0,608
DefineTextStyle id=6 font=5 size=100
TextOut 'Reserved For Class Logo' style=6 at 0,0
Show
send

0000  ff ff ff ff ff ff 00 11  d8 b5 a2 a2 08 00 45 00   ........ ......E.
0010  02 fc 36 fe 00 00 80 11  40 49 c0 a8 00 02 ff ff   ..6..... @I......
0020  ff ff 05 69 23 f0 02 e8  94 ec 00 03 e9 02 db 01   ...i#... ........
0030  01 00 00 01 01 01 01 00  00 02 00 ff 05 02 00 01   ........ ........
0040  00 0e 00 00 00 00 00 05  50 03 00 ff 00 00 00 02   ........ P.......
0050  05 03 00 01 01 00 0e 00  00 00 00 00 05 50 00 a0   ........ .....P..
0060  ff 00 00 00 02 0b 01 01  00 10 02 0e 74 37 31 33   ........ ....t713
0070  36 30 78 31 36 30 2e 67  69 66 0b 04 00 01 01 01   60x160.g if......
0080  00 19 00 00 00 00 00 05  50 00 a0 02 0e 74 37 31   ........ P....t71
0090  33 36 30 78 31 36 30 2e  67 69 66 05 03 00 01 02   360x160. gif.....
00a0  00 0e 00 00 00 00 a0 05  50 00 96 ff 00 00 00 02   ........ P.......
00b0  04 01 05 00 0d 00 0b 73  77 69 73 73 62 63 2e 74   .......s wissbc.t
00c0  74 66 02 01 01 00 0a 00  01 05 82 cd ff 00 00 01   tf...... ........
00d0  00 05 04 00 01 02 01 00  0e 00 00 00 00 00 02 8a   ........ ........
00e0  00 96 ff 00 00 00 02 07  05 00 01 02 01 01 00 15   ........ ........
00f0  00 01 45 00 8b 01 01 02  06 00 54 00 37 00 20 00   ..E..... ..T.7. .
0100  38 00 39 00 39 02 01 02  00 0a 00 01 05 28 00 ff   8.9.9... .....(..
0110  00 00 00 00 05 04 00 01  02 02 00 0e 00 02 8a 00   ........ ........
0120  23 01 f4 00 37 ff 00 00  00 02 07 05 00 01 02 02   #...7... ........
0130  01 00 27 00 00 03 00 2b  01 02 02 0f 00 44 00 65   ..'....+ .....D.e
0140  00 70 00 61 00 72 00 74  00 75 00 72 00 65 00 20   .p.a.r.t .u.r.e. 
0150  00 54 00 69 00 6d 00 65  00 3a 02 01 03 00 0a 00   .T.i.m.e .:......
0160  01 05 28 00 ff 00 00 01  00 05 04 00 01 02 03 00   ..(..... ........
0170  0e 00 04 7e 00 23 00 5a  00 37 ff 00 00 00 02 07   ...~.#.Z .7......
0180  05 00 01 02 03 01 00 11  00 00 2d 00 2b 01 03 02   ........ ..-.+...
0190  04 00 30 00 30 00 30 00  38 05 04 00 01 02 04 00   ..0.0.0. 8.......
01a0  0e 00 02 8a 00 5f 01 f4  00 37 ff 00 00 00 02 07   ....._.. .7......
01b0  05 00 01 02 04 01 00 2b  00 00 03 00 2b 01 02 02   .......+ ....+...
01c0  11 00 42 00 6f 00 61 00  72 00 64 00 69 00 6e 00   ..B.o.a. r.d.i.n.
01d0  67 00 20 00 61 00 74 00  20 00 47 00 61 00 74 00   g. .a.t.  .G.a.t.
01e0  65 00 3a 02 01 04 00 0a  00 01 05 28 ff 03 00 00   e.:..... ...(....
01f0  01 00 05 04 00 01 02 05  00 0e 00 04 7e 00 5f 00   ........ ....~._.
0200  5a 00 37 03 00 00 00 02  07 05 00 01 02 05 01 00   Z.7..... ........
0210  0b 00 00 2d 00 2b 01 04  02 01 00 31 05 03 00 01   ...-.+.. ...1....
0220  04 00 0e 00 00 00 01 36  05 50 01 2a ff 00 00 00   .......6 .P.*....
0230  02 02 01 05 00 0a 00 01  05 64 00 ff 00 00 01 00   ........ .d......
0240  05 04 00 01 04 01 00 0e  00 00 00 00 00 05 50 01   ........ ......P.
0250  2a ff 00 00 00 02 07 05  00 01 04 01 01 00 2b 00   *....... ......+.
0260  02 a8 00 b9 01 05 02 11  00 41 00 64 00 65 00 6c   ........ .A.d.e.l
0270  00 61 00 69 00 64 00 65  00 20 00 43 00 68 00 69   .a.i.d.e . .C.h.i
0280  00 63 00 6c 00 61 00 79  00 6f 05 03 00 01 06 00   .c.l.a.y .o......
0290  0e 00 00 00 02 60 05 50  00 a0 ce 00 00 00 02 02   .....`.P ........
02a0  01 06 00 0a 00 01 05 64  ff ce 00 00 01 00 05 04   .......d ........
02b0  00 01 06 01 00 0e 00 00  00 00 00 05 50 00 a0 ce   ........ ....P...
02c0  00 00 00 02 07 05 00 01  06 01 01 00 37 00 02 a8   ........ ....7...
02d0  00 6b 01 06 02 17 00 52  00 65 00 73 00 65 00 72   .k.....R .e.s.e.r
02e0  00 76 00 65 00 64 00 20  00 46 00 6f 00 72 00 20   .v.e.d.  .F.o.r. 
02f0  00 43 00 6c 00 61 00 73  00 73 00 20 00 4c 00 6f   .C.l.a.s .s. .L.o
0300  00 67 00 6f 01 01 00 00  01 03                     .g.o.... ..      

TfrFile C:\Usr\FIDSxml\graphics\gifs\airlines\1360x160\t71360x160.gif
send
send
send
send
send
send
send
Update
send

0000  ff ff ff ff ff ff 00 11  d8 b5 a2 a2 08 00 45 00   ........ ......E.
0010  00 3a 36 ff 00 00 80 11  43 0a c0 a8 00 02 ff ff   .:6..... C.......
0020  ff ff 05 69 23 f0 00 26  d7 c2 00 03 e9 00 19 03   ...i#..& ........
0030  01 00 00 14 00 0e 74 37  31 33 36 30 78 31 36 30   ......t7 1360x160
0040  2e 67 69 66 00 00 18 e9                            .gif....         

0000  ff ff ff ff ff ff 00 11  d8 b5 a2 a2 08 00 45 00   ........ ......E.
0010  04 77 37 00 00 00 80 11  3e cc c0 a8 00 02 ff ff   .w7..... >.......
0020  ff ff 05 69 23 f0 04 63  b4 60 00 03 e9 04 56 03   ...i#..c .`....V.
0030  01 00 04 51 02 00 00 04  4c 47 49 46 38 39 61 50   ...Q.... LGIF89aP
0040  05 a0 00 f7 00 00 00 00  00 00 00 44 00 00 88 00   ........ ...D....
0050  00 cc 00 44 00 00 44 44  00 44 88 00 44 cc 00 88   ...D..DD .D..D...   etc.....etc for gif down load pkts
..........

0000  ff ff ff ff ff ff 00 11  d8 b5 a2 a2 08 00 45 00   ........ ......E.
0010  00 27 37 06 00 00 80 11  43 16 c0 a8 00 02 ff ff   .'7..... C.......
0020  ff ff 05 69 23 f0 00 13  0b bf 00 03 e9 00 06 01   ...i#... ........
0030  01 00 00 01 1a                                     .....               1a = aUpdate


eg 2 line departures screen as frame 1

id=0 fr=1001 len=5  PAGE 0 Show |
00 03 E9 00 05 01 00 00 01 03
id=0 fr=1001 len=1203  PAGE 0 Del | PAGE 0 New | WINDOW 0.1 New x=0 y=0 1360x768 |
WINDOW 0.1.1 New x=0 y=0 1360x60 | WINDOW 0.1.1.1 New x=0 y=0 150x60 | FONT 5 New swissbc.ttf |
TEXTSTYLE 1 New 5 size=55 | WINDOW 0.1.1.2 New x=150 y=0 1060x60 | UTEXT 0.1.1.2.1 New 1 Put "DEPARTURES" |
TEXTSTYLE 2 New 5 size=34 | WINDOW 0.1.1.3 New x=1210 y=0 150x60 | UTEXT 0.1.1.3.1 New 2 Put "07:15" |
WINDOW 0.1.2 New x=0 y=60 1360x40 | TEXTSTYLE 3 New 5 size=34 | WINDOW 0.1.2.1 New x=0 y=0 240x40 |
UTEXT 0.1.2.1.1 New 3 Put "Airline" | WINDOW 0.1.2.2 New x=240 y=0 215x40 | UTEXT 0.1.2.2.1 New 3 Put "Flight" |
WINDOW 0.1.2.3 New x=455 y=0 430x40 | UTEXT 0.1.2.3.1 New 3 Put "Destination" | WINDOW 0.1.2.4 New x=885 y=0 115x40 |
UTEXT 0.1.2.4.1 New 3 Put "Schd" | WINDOW 0.1.2.5 New x=1000 y=0 115x40 | UTEXT 0.1.2.5.1 New 3 Put "Estm" |
WINDOW 0.1.2.6 New x=1115 y=0 165x40 | UTEXT 0.1.2.6.1 New 3 Put "Status" | WINDOW 0.1.2.7 New x=1280 y=0 80x40 |
UTEXT 0.1.2.7.1 New 2 Put "Gate" | WINDOW 0.1.3 New x=0 y=100 1360x1 | WINDOW 0.1.3.1 New x=0 y=0 1360x1 |
WINDOW 0.1.6 New x=0 y=101 1360x60 | IMAGE 1 Put qf230x60.gif | IMAGE 2 Put gt230x60.gif |
IMAGE 3 Put nz230x60.gif | VSCROLL 0.1.6.1 New x=0 y=0 230x60 | IMAGE 0.1.6.1.0.0 New 230x60 Put qf230x60.gif |
IMAGE 0.1.6.1.1.0 New 230x60 Put gt230x60.gif | IMAGE 0.1.6.1.2.0 New 230x60 Put nz230x60.gif |
FONT 4 New swissm.ttf | TEXTSTYLE 4 New 4 size=44 | VSCROLL 0.1.6.2 New x=230 y=0 225x60 |
UTEXT 0.1.6.2.0.0 New 4 Put "QF 123" | UTEXT 0.1.6.2.1.0 New 4 Put "GT 195" | UTEXT 0.1.6.2.2.0 New 4 Put "NZ 8459" |
TEXTSTYLE 5 New 5 size=44 | WINDOW 0.1.6.3 New x=455 y=0 430x60 | UTEXT 0.1.6.3.1 New 5 Put "Brisbane" |
WINDOW 0.1.6.4 New x=885 y=0 115x60 | UTEXT 0.1.6.4.1 New 5 Put "0330" | TEXTSTYLE 6 New 5 size=44 |
WINDOW 0.1.6.5 New x=1000 y=0 115x60 | UTEXT 0.1.6.5.1 New 6 Put "0330" | WINDOW 0.1.6.6 New x=1115 y=0 165x60 |

id=0 fr=1001 len=518  TEXTSTYLE 7 New 5 size=44 | WINDOW 0.1.6.7 New x=1280 y=0 80x60 |
UTEXT 0.1.6.7.1 New 7 Put "5" | WINDOW 0.1.7 New x=0 y=161 1360x2 | WINDOW 0.1.7.1 New x=0 y=0 1360x2 |
WINDOW 0.1.10 New x=0 y=163 1360x60 | IMAGE 4 Put st230x60.gif | IMAGE 0.1.10.1 New 230x60 Put st230x60.gif |
TEXTSTYLE 8 New 4 size=44 | WINDOW 0.1.10.2 New x=230 y=0 225x60 | UTEXT 0.1.10.2.1 New 8 Put "ST 1234" |
TEXTSTYLE 9 New 5 size=44 | WINDOW 0.1.10.3 New x=455 y=0 430x60 | UTEXT 0.1.10.3.1 New 9 Put "Perth" |
WINDOW 0.1.10.4 New x=885 y=0 115x60 | UTEXT 0.1.10.4.1 New 9 Put "0700" | TEXTSTYLE 10 New 5 size=44 |
WINDOW 0.1.10.5 New x=1000 y=0 115x60 | UTEXT 0.1.10.5.1 New 10 Put "0700" | WINDOW 0.1.10.6 New x=1115 y=0 165x60 |
WINDOW 0.1.10.7 New x=1280 y=0 80x60 | UTEXT 0.1.10.7.1 New 7 Put "13" | WINDOW 0.1.11 New x=0 y=223 1360x2 |
WINDOW 0.1.11.1 New x=0 y=0 1360x2 | PAGE 0 Show |

00 03 E9 04 B3 01 00 00 01 01 01 00 00 02 00 FF 05 02 00 01 00 0D 00 00 00 00 00 05 50 03 00 FF 00 00 00 05 03 00 01 01 00 0D 00 00 00 00 00 05
50 00 3C E9 00 00 00 05 04 00 01 01 01 00 0D 00 00 00 00 00 00 96 00 3C E9 00 00 00 04 01 05 00 0D 00 0B 73 77 69 73 73 62 63 2E 74 74 66 02 01
01 00 0A 00 01 05 37 00 E9 00 00 01 00 05 04 00 01 01 02 00 0D 00 00 96 00 00 04 24 00 3C E9 00 00 00 07 05 00 01 01 02 01 00 1D 00 02 12 00 3B
01 01 02 0A 00 44 00 45 00 50 00 41 00 52 00 54 00 55 00 52 00 45 00 53 02 01 02 00 0A 00 01 05 22 00 E9 00 00 01 00 05 04 00 01 01 03 00 0D 00
04 BA 00 00 00 96 00 3C E9 00 00 00 07 05 00 01 01 03 01 00 13 00 00 4B 00 24 01 02 02 05 00 30 00 36 00 3A 00 34 00 33 05 03 00 01 02 00 0D 00
00 00 00 3C 05 50 00 28 E9 00 00 00 02 01 03 00 0A 00 01 05 22 00 E9 00 00 00 00 05 04 00 01 02 01 00 0D 00 00 00 00 00 00 F0 00 28 E9 00 00 00
07 05 00 01 02 01 01 00 17 00 00 0A 00 24 01 03 02 07 00 41 00 69 00 72 00 6C 00 69 00 6E 00 65 05 04 00 01 02 02 00 0D 00 00 F0 00 00 00 D7 00
28 E9 00 00 00 07 05 00 01 02 02 01 00 15 00 00 03 00 24 01 03 02 06 00 46 00 6C 00 69 00 67 00 68 00 74 05 04 00 01 02 03 00 0D 00 01 C7 00 00
01 AE 00 28 E9 00 00 00 07 05 00 01 02 03 01 00 1F 00 00 03 00 24 01 03 02 0B 00 44 00 65 00 73 00 74 00 69 00 6E 00 61 00 74 00 69 00 6F 00 6E
05 04 00 01 02 04 00 0D 00 03 75 00 00 00 73 00 28 E9 00 00 00 07 05 00 01 02 04 01 00 11 00 00 03 00 24 01 03 02 04 00 53 00 63 00 68 00 64 05
04 00 01 02 05 00 0D 00 03 E8 00 00 00 73 00 28 E9 00 00 00 07 05 00 01 02 05 01 00 11 00 00 03 00 24 01 03 02 04 00 45 00 73 00 74 00 6D 05 04
00 01 02 06 00 0D 00 04 5B 00 00 00 A5 00 28 E9 00 00 00 07 05 00 01 02 06 01 00 15 00 00 03 00 24 01 03 02 06 00 53 00 74 00 61 00 74 00 75 00
73 05 04 00 01 02 07 00 0D 00 05 00 00 00 00 50 00 28 E9 00 00 00 07 05 00 01 02 07 01 00 11 00 00 28 00 24 01 02 02 04 00 47 00 61 00 74 00 65
05 03 00 01 03 00 0D 00 00 00 00 64 05 50 00 01 03 00 00 00 05 04 00 01 03 01 00 0D 00 00 00 00 00 05 50 00 01 03 00 00 00 05 03 00 01 06 00 0D
00 00 00 00 65 05 50 00 3C FF 00 00 00 0B 01 01 00 0E 02 0C 71 66 32 33 30 78 36 30 2E 67 69 66 0B 01 02 00 0E 02 0C 67 74 32 33 30 78 36 30 2E
67 69 66 0B 01 03 00 0E 02 0C 6E 7A 32 33 30 78 36 30 2E 67 69 66 0D 04 00 01 06 01 00 0C 00 00 00 00 00 00 E6 00 3C 03 FF 00 0B 06 00 01 06 01
00 00 00 17 00 00 00 00 00 00 E6 00 3C 02 0C 71 66 32 33 30 78 36 30 2E 67 69 66 0B 06 00 01 06 01 01 00 00 17 00 00 00 00 00 00 E6 00 3C 02 0C
67 74 32 33 30 78 36 30 2E 67 69 66 0B 06 00 01 06 01 02 00 00 17 00 00 00 00 00 00 E6 00 3C 02 0C 6E 7A 32 33 30 78 36 30 2E 67 69 66 04 01 04
00 0C 00 0A 73 77 69 73 73 6D 2E 74 74 66 02 01 04 00 0A 00 01 04 2C CD FF 00 00 00 00 0D 04 00 01 06 02 00 0C 00 00 E6 00 00 00 E1 00 3C 03 FF
00 07 06 00 01 06 02 00 00 00 15 00 00 0A 00 2F 01 04 02 06 00 51 00 46 00 20 00 31 00 32 00 33 07 06 00 01 06 02 01 00 00 15 00 00 0A 00 2F 01
04 02 06 00 47 00 54 00 20 00 31 00 39 00 35 07 06 00 01 06 02 02 00 00 17 00 00 0A 00 2F 01 04 02 07 00 4E 00 5A 00 20 00 38 00 34 00 35 00 39
02 01 05 00 0A 00 01 05 2C 00 FF 00 00 00 00 05 04 00 01 06 03 00 0D 00 01 C7 00 00 01 AE 00 3C FF 00 00 00 07 05 00 01 06 03 01 00 19 00 00 03
00 2F 01 05 02 08 00 42 00 72 00 69 00 73 00 62 00 61 00 6E 00 65 05 04 00 01 06 04 00 0D 00 03 75 00 00 00 73 00 3C FF 00 00 00 07 05 00 01 06
04 01 00 11 00 00 03 00 2F 01 05 02 04 00 30 00 33 00 33 00 30 02 01 06 00 0A 00 01 05 2C 03 FF 00 00 00 00 05 04 00 01 06 05 00 0D 00 03 E8 00
00 00 73 00 3C FF 00 00 00 07 05 00 01 06 05 01 00 11 00 00 03 00 2F 01 06 02 04 00 30 00 33 00 33 00 30 05 04 00 01 06 06 00 0D 00 04 5B 00 00
00 A5 00 3C FF 00 00 00
00 03 E9 02 06 02 01 07 00 0A 00 01 05 2C FF 03 00 00 01 00 05 04 00 01 06 07 00 0D 00 05 00 00 00 00 50 00 3C 03 00 00 00 07 05 00 01 06 07 01
00 0B 00 00 28 00 2F 01 07 02 01 00 35 05 03 00 01 07 00 0D 00 00 00 00 A1 05 50 00 02 68 00 00 00 05 04 00 01 07 01 00 0D 00 00 00 00 00 05 50
00 02 68 00 00 00 05 03 00 01 0A 00 0D 00 00 00 00 A3 05 50 00 3C BB 00 00 00 0B 01 04 00 0E 02 0C 73 74 32 33 30 78 36 30 2E 67 69 66 0B 04 00
01 0A 01 00 17 00 00 00 00 00 00 E6 00 3C 02 0C 73 74 32 33 30 78 36 30 2E 67 69 66 02 01 08 00 0A 00 01 04 2C CD BB 00 00 00 00 05 04 00 01 0A
02 00 0D 00 00 E6 00 00 00 E1 00 3C BB 00 00 00 07 05 00 01 0A 02 01 00 17 00 00 0A 00 2F 01 08 02 07 00 53 00 54 00 20 00 31 00 32 00 33 00 34
02 01 09 00 0A 00 01 05 2C 00 BB 00 00 00 00 05 04 00 01 0A 03 00 0D 00 01 C7 00 00 01 AE 00 3C BB 00 00 00 07 05 00 01 0A 03 01 00 13 00 00 03
00 2F 01 09 02 05 00 50 00 65 00 72 00 74 00 68 05 04 00 01 0A 04 00 0D 00 03 75 00 00 00 73 00 3C BB 00 00 00 07 05 00 01 0A 04 01 00 11 00 00
03 00 2F 01 09 02 04 00 30 00 37 00 30 00 30 02 01 0A 00 0A 00 01 05 2C 03 BB 00 00 00 00 05 04 00 01 0A 05 00 0D 00 03 E8 00 00 00 73 00 3C BB
00 00 00 07 05 00 01 0A 05 01 00 11 00 00 03 00 2F 01 0A 02 04 00 30 00 37 00 30 00 30 05 04 00 01 0A 06 00 0D 00 04 5B 00 00 00 A5 00 3C BB 00
00 00 05 04 00 01 0A 07 00 0D 00 05 00 00 00 00 50 00 3C 03 00 00 00 07 05 00 01 0A 07 01 00 0D 00 00 28 00 2F 01 07 02 02 00 31 00 33 05 03 00
01 0B 00 0D 00 00 00 00 DF 05 50 00 02 68 00 00 00 05 04 00 01 0B 01 00 0D 00 00 00 00 00 05 50 00 02 68 00 00 00 01 00 00 01 03

eg download QF230x60.gif

id=0 fr=1001 len=22  FILE 0 New qf230x60.gif size=2097  |
00 03 E9 00 16 03 00 00 12 00 0C 71 66 32 33 30 78 36 30 2E 67 69 66 00 00 08 31
id=0 fr=1001 len=1109  FILE 0 Put size=1100 |
00 03 E9 04 55 03 00 04 51 02 00 00 04 4C 47 49 46 38 39 61 E6 00 3C 00 F7 00 00 00 00 00 00 00 44 00 00 88 00 00 CC 00 44 00 00 44 44 00 44 88
00 44 CC 00 88 00 00 88 44 00 88 88 00 88 CC 00 CC 00 00 CC 44 00 CC 88 00 CC CC 00 DD DD 11 11 11 00 00 55 00 00 99 00 00 DD 00 55 00 00 55 55
00 4C 99 00 49 DD 00 99 00 00 99 4C 00 99 99 00 93 DD 00 DD 00 00 DD 49 00 DD 93 00 EE 9E 00 EE EE 22 22 22 00 00 66 00 00 AA 00 00 EE 00 66 00
00 66 66 00 55 AA 00 4F EE 00 AA 00 00 AA 55 00 AA AA 00 9E EE 00 EE 00 00 EE 4F 00 FF 55 00 FF AA 00 FF FF 33 33 33 00 00 77 00 00 BB 00 00 FF
00 77 00 00 77 77 00 5D BB 00 55 FF 00 BB 00 00 BB 5D 00 BB BB 00 AA FF 00 FF 00 44 00 44 44 00 88 44 00 CC 44 44 00 44 44 44 44 44 88 44 44 CC
44 88 00 44 88 44 44 88 88 44 88 CC 44 CC 00 44 CC 44 44 CC 88 44 CC CC 44 00 00 55 00 00 55 00 55 4C 00 99 49 00 DD 55 55 00 55 55 55 4C 4C 99
49 49 DD 4C 99 00 4C 99 4C 4C 99 99 49 93 DD 49 DD 00 49 DD 49 49 DD 93 49 DD DD 4F EE EE 66 00 00 66 00 66 55 00 AA 4F 00 EE 66 66 00 66 66 66
55 55 AA 4F 4F EE 55 AA 00 55 AA 55 55 AA AA 4F 9E EE 4F EE 00 4F EE 4F 4F EE 9E 55 FF AA 55 FF FF 77 00 00 77 00 77 5D 00 BB 55 00 FF 77 77 00
77 77 77 5D 5D BB 55 55 FF 5D BB 00 5D BB 5D 5D BB BB 55 AA FF 55 FF 00 55 FF 55 88 00 88 88 00 CC 88 44 00 88 44 44 88 44 88 88 44 CC 88 88 00
88 88 44 88 88 88 88 88 CC 88 CC 00 88 CC 44 88 CC 88 88 CC CC 88 00 00 88 00 44 99 00 4C 99 00 99 93 00 DD 99 4C 00 99 4C 4C 99 4C 99 93 49 DD
99 99 00 99 99 4C 99 99 99 93 93 DD 93 DD 00 93 DD 49 93 DD 93 93 DD DD 99 00 00 AA 00 00 AA 00 55 AA 00 AA 9E 00 EE AA 55 00 AA 55 55 AA 55 AA
9E 4F EE AA AA 00 AA AA 55 AA AA AA 9E 9E EE 9E EE 00 9E EE 4F 9E EE 9E 9E EE EE AA FF FF BB 00 00 BB 00 5D BB 00 BB AA 00 FF BB 5D 00 BB 5D 5D
BB 5D BB AA 55 FF BB BB 00 BB BB 5D BB BB BB AA AA FF AA FF 00 AA FF 55 AA FF AA CC 00 CC CC 44 00 CC 44 44 CC 44 88 CC 44 CC CC 88 00 CC 88 44
CC 88 88 CC 88 CC CC CC 00 CC CC 44 CC CC 88 CC CC CC CC 00 00 CC 00 44 CC 00 88 DD 00 93 DD 00 DD DD 49 00 DD 49 49 DD 49 93 DD 49 DD DD 93 00
DD 93 49 DD 93 93 DD 93 DD DD DD 00 DD DD 49 DD DD 93 DD DD DD DD 00 00 DD 00 49 EE 00 4F EE 00 9E EE 00 EE EE 4F 00 EE 4F 4F EE 4F 9E EE 4F EE
EE 9E 00 EE 9E 4F EE 9E 9E EE 9E EE EE EE 00 EE EE 4F EE EE 9E EE EE EE EE 00 00 FF 00 00 FF 00 55 FF 00 AA FF 00 FF FF 55 00 FF 55 55 FF 55 AA
FF 55 FF FF AA 00 FF AA 55 FF AA AA FF AA FF FF FF 00 FF FF 55 FF FF AA FF FF FF 2C 00 00 00 00 E6 00 3C 00 00 08 FF 00 FF 09 1C 48 B0 A0 C1 83
08 13 2A 5C C8 B0 A1 C3 87 10 23 4A 9C 48 B1 A2 C5 8B 18 33 6A DC C8 B1 E3 C6 7A 1E 43 8A 1C 49 B2 24 C9 77 20 4D AA 5C C9 B2 A5 CA 77 28 5D CA
9C 49 B3 26 43 98 31 6D EA DC C9 B3 24 CE 9C 3D 83 0A 1D 1A F1 27 50 A2 48 93 26 35 7A 54 A9 D3 A7 35 99 36 85 4A B5 EA 49 A9 29 AD 6A DD BA 51
2A CC AC 5C C3 8A 85 E8 F5 EB D8 B3 68 13 96 35 9B B6 6D DA B5 6C DD CA E5 0A 37 EE DC BB 50 EB DA C5 CB 97 A8 DE BD 7D 03 EF FC 0B 58 B0 61 99
84 0B 1F 5E 6C 32 B1 62 C6 90 3D 3A 7E 1C B9 32 C6 C9 94 2D 6B 96 88 39 73 48 66 99 EE DC 21 42 9A 48 15 D1 88 76 B9 DB DC B1 1B 22 33 44 22 00
98 3D 7B 06 11 33 77 54 31 7B D8 D9 B3 46 55 55 64 D3 1E 4E 1C C0 0C 44 AB 17 CE 28 CE 7C F6 C1 6E CD 69 27 17 A8 AA B9 99 7F 44 A2 6B 27 3E 63
60 37 E1 DB 01 10 FF 39 98 69 79 F8 E1 44 EE DC EC 3D B5 A3 2A 11 E7 8B 47 C8 A4 F0 4E 7C E7 06 ED 6B A7 3F D0 4C 73 55 D0 DD 17 1D 22 03 99 77
DE 78 04 B9 93 9D 80 B4 89 B0
id=0 fr=1001 len=1006  FILE 0 Put size=997 |
00 03 E9 03 EE 03 00 03 EA 02 00 01 03 E5 1E 7B 60 71 E4 4E 15 0C 16 C7 9F 41 D5 09 78 10 7C DA 55 31 90 3B E0 0D E7 20 22 15 32 D7 8D 40 FA C5
87 A0 40 DF 95 38 9B 7A 0B B1 87 53 84 19 B9 63 A0 8B B3 9D 58 50 8B 1A 16 C4 CC 79 C9 65 D2 9C 7A 37 BA 88 60 86 F7 AD F8 4F 91 0C EE B6 50 37
E4 C8 D8 9E 45 4C BA 78 5D 41 55 6E 97 DF 79 FC 51 C8 1C 33 01 E2 38 1B 7D 20 32 B8 A2 90 DA CD 20 DA 68 0B 36 D8 D0 34 EE A4 23 25 8D 15 A5 18
9D 08 44 64 29 62 41 76 12 C7 21 73 06 FD D9 E1 3F EE 34 37 A2 98 D2 61 77 67 73 2B EA 39 9B 2A 06 75 A3 CA 68 30 C6 08 A7 3B D8 C8 48 A7 44 61
0E 39 9D 3B 7D 16 47 D0 2E D1 45 F0 63 73 05 21 19 9E 3B 68 16 A7 9E A0 2E 7A FF 18 2A 6D 33 A8 8A DE 40 F1 D9 76 47 26 4E 92 05 93 8E FA D4 D3
DB A6 10 F9 D7 9C A9 07 19 8B AA 40 65 36 47 5F 74 05 29 1B 5E 26 5E 16 B7 DB 9A D8 B6 59 5C 7A D8 8A B6 0B A9 C7 32 03 6E 71 DD 09 54 A2 08 77
E8 D8 D0 4F F5 7C AA 0F 36 F5 90 23 EF 5F C4 3A 14 22 71 04 1E 34 EB 6C 11 0C A4 2D 71 30 46 A7 EE 3F F7 A6 D9 5C B9 07 FD 3B 5C AF 09 16 4C 1B
A4 E3 8A 6A AE 8B F3 39 C4 14 36 D3 1D A4 4F 3A C2 96 55 EF 42 11 FB 99 90 B4 C4 21 B8 AF 78 B8 36 B7 CB 40 B6 BA 98 6F A4 77 22 54 AD AB 02 85
3C 5C 7F 62 42 FA A0 51 F5 E8 B3 10 A6 1E 53 44 E2 90 08 35 CB 9C 7A 36 0F 87 6D 74 2B 0B 44 32 6D 0E 0B 8C D0 D0 47 1F 44 B5 7C 6B 3E 4D DB 87
8E 6E 87 B0 42 70 D5 33 B0 41 EE 44 89 D5 44 27 57 CA A7 76 CC B8 03 6B 85 4D 17 CA 9C 08 5A 1F 9C 90 9E 63 FF 73 AA 98 7C 46 FF 1D DE BA 7A A5
A3 90 9C 38 61 03 EF 94 F5 45 A7 E4 40 27 A3 3C B3 8B 4D B7 0A 70 CB 03 22 D4 29 77 64 BF 5D E1 41 93 2A AC E5 CE 75 F9 6C 39 4C 82 0F 34 CD C7
5B 2A AE CA B7 BB DC A1 F9 70 BB 5C 8D 63 D3 8F D3 B6 9B DF C4 65 4C D0 C9 2F 0F 54 7B 89 35 57 81 08 C3 84 C6 5E F7 CD A0 D7 55 FA 41 65 BF 93
9C 3E BE 0D 24 39 A2 77 EC 8D E8 CA 72 17 E7 E0 3F C7 0F E7 21 42 AF 03 30 B6 EC 38 3A 0D 75 7A A9 ED 72 A2 8D 69 02 9E D8 F2 09 35 3F E3 43 D9
23 6A 86 DB 88 D2 B6 F2 F4 B4 C1 C8 3F 73 17 F2 51 74 BE 37 10 66 E0 AE 47 07 0C 4F EF D4 32 19 DD 19 84 70 EC 7A 48 E3 E2 73 9D E3 E1 A9 34 18
64 DA 92 9A A3 A3 FA 45 C7 81 28 8A 4E 00 D9 77 30 0C 96 06 6F 94 13 D0 D7 C0 36 99 08 65 AC 1B 5E 41 1D CE 5C 24 02 48 FD 8F 5F 20 FC C7 80 2E
47 2B 82 FC CE 7B 09 09 5F C6 FF 8E B7 C2 81 24 6D 36 BB E8 5E 78 90 E5 3E C7 24 C7 1D 62 EB 46 3A 36 B6 16 19 0A 04 11 09 64 8E 87 0C A8 41 84
44 E7 0E E4 9B 4D EF 6E F8 30 84 1C 11 00 04 FC 07 19 23 90 C3 33 EE 22 7C E1 A9 42 0E 0F D2 99 D2 91 23 1D 10 AC 8B 15 09 45 AD 2C 0E A7 73 26
2C CD 02 0B 12 C8 D2 84 A6 90 44 18 98 82 10 49 9A 84 64 82 91 4D 13 48 15 20 99 10 66 30 72 37 DD 38 64 16 23 60 06 E2 25 8F 5E 02 C1 CC 1E BD
F3 AD D0 64 E2 5B 3F 64 23 6B 30 E2 8E 5D 4C AA 5B A2 D1 0D 45 3A 43 8E 50 8A B2 46 51 4B E3 2A 85 D2 99 69 D8 F2 96 BB AC 4C 6F 04 D7 31 60 06
93 31 EC E9 06 0C 87 75 4C 64 F6 C6 97 66 EB CC 28 9B 29 16 19 61 4C 53 D4 14 8C 94 DC 01 3D 08 65 B3 2F 52 F2 65 1E 8D F9 4D B9 48 29 26 E3 6C
61 39 E7 72 CE 77 08 2E 9D 8E 99 E6 3A 95 D2 4E 77 FE 23 9A D2 9C E7 5B EA 99 16 0E 77 CC 49 9F 67 A9 27 E9 E0 19 4F 80 1A F4 A0 08 4D E8 66 02
02 00 3B

id=0 fr=0 len=10  DISPLAY 0 SetIODev type=1 port=9200 1 |
00 00 00 00 0A 00 00 00 06 21 01 23 F0 01 01
id=0 fr=0 len=34  DISPLAY 0 PutIODev "01HELLO out there 00000000" |
00 00 00 00 22 00 00 00 1E 22 1C 30 31 48 45 4C 4C 4F 20 6F 75 74 20 74 68 65 72 65 20 30 30 30 30 30 30 30 30 0D 03

id=0 fr=0 len=15  DISPLAY 0 Frames 1001/0 | DISPLAY 0 Carousel |
00 00 00 00 0F 00 00 00 05 0E 01 03 E9 00 00 00 00 02 15 00
id=0 fr=0 len=71  PAGE 0 Del | PAGE 0 New | TEXTSTYLE 1 New 0 size=42 | UTEXT 0.1 New 1 Put "DII UDC frame 1" |
00 00 00 00 47 01 00 00 01 01 01 00 00 02 00 C9 02 01 01 00 0A 00 01 00 2A 00 00 00 00 00 00 07 02 00 01 00 27 00 00 14 00 3C 01 01 02 0F 00 44
id=0 fr=1001 len=5  PAGE 0 Show |
00 03 E9 00 05 01 00 00 01 03

00 49 00 49 00 20 00 55 00 44 00 43 00 20 00 66 00 72 00 61 00 6D 00 65 00 20 00 31
id=0 fr=0 len=11  DISPLAY 0 Sip 192.168.0.104 : 9200 |
00 00 00 00 0B 00 00 00 07 0F C0 A8 00 68 23 F0
id=0 fr=0 len=10  DISPLAY 0 SetIODev type=1 port=9200 1 |
00 00 00 00 0A 00 00 00 06 21 01 23 F0 01 01
id=0 fr=0 len=20  DISPLAY 0 PutIODev "011234567890" |00 00 00 00 14 00 00 00 10 22 0E 30 31 31 32 33 34 35 36 37 38 39 30 0D 03
COM3 1078.7  01<1B>p<03>011234567890<0D><03>


TCP Connection from 192.168.0.20
00 00 54 47 54 00 43 00 50 00 20 00 44 00 61 00 74 00 61 00 20 00 66 00 72 00 6F 00 6D 00 20 00 31 00 39 00 32 00 2E 00 31 00 36 00 38 00 2E 00
30 00 2E 00 32 00 30 00 00 00 00 00


FIDSm2 page draw
id=1 fr=1064 len=907  PAGE 0 New Name "CHKIN1SYX1360X768" | WINDOW 0.1 New x=0 y=0 1360x160 |
WINDOW 0.1.1 New x=0 y=0 1360x160 | WINDOW 0.1.1.0 New x=0 y=0 1360x160 | IMAGE 0.1.1.0.0 New 1360x160 Put 1360x160\VA.gif |
IMAGE 0.1.1.1.0 New 1360x160 | IMAGE 0.1.1.2.0 New 1360x160 | IMAGE 0.1.1.3.0 New 1360x160 |
IMAGE 0.1.1.4.0 New 1360x160 | WINDOW 0.2 New x=0 y=160 1360x373 | WINDOW 0.2.0 New x=0 y=0 600x165 |
WINDOW 0.2.0.0 New x=0 y=0 600x165 | TEXT 0.2.0.0.0 New x=77 y=155  48 Put "VA 123" |
UTEXT 0.2.1 New x=339 y=340  72 Put "AA" | UTEXT 0.2.2 New x=969 y=340  202 PutAttr 4 sz=120 Put "AA" |
UTEXT 0.2.3.1 New x=400 y=90  70 Put "AA" | UTEXT 0.2.3.2 New x=1080 y=90  18 PutAttr 4 sz=100 Put "AA" |
TEXT 0.2.5 New x=650 y=70  184 PutAttr 2 sz=45 Put "Departure Time" | UTEXT 0.2.6 New x=965 y=70  184 PutAttr 4 sz=48 Put "起飞时间:" |
TEXT 0.2.7 New x=1186 y=70  216 Put "0600" | TEXT 0.2.8 New x=650 y=155  184 PutAttr 2 sz=45 Put "Boarding at Gate" |
UTEXT 0.2.9 New x=1012 y=155  184 PutAttr 4 sz=48 Put "登机门:" | WINDOW 0.2.10 New x=1215 y=105 90x65 |
TEXT 0.2.10.1 New x=30 y=55  243 Put "3" | WINDOW 0.10 New x=0 y=587 1360x180 | WINDOW 0.10.1 New x=0 y=20 1360x160 |
IMAGE 0.10.1.1 New 1360x160 Put 1360x160\VA_EC.gif | WINDOW 0.10.4.2 New x=0 y=80 1360x100 |
IMAGE 0.10.4.2.1 New 1360x100 Put 1360x100\VA_EC.gif | WINDOW 0.10.5.2 New x=0 y=80 1360x100 |
IMAGE 0.10.5.2.1 New 1360x100 Put 1360x100\VA_EC.gif | WINDOW 0.10.6.2 New x=0 y=80 1360x100 |
IMAGE 0.10.6.2.1 New 1360x100 Put 1360x100\VA_EC.gif | WINDOW 0.10.7.2 New x=0 y=80 1360x100 |
IMAGE 0.10.7.2.1 New 1360x100 Put 1360x100\VA_EC.gif | PAGE 0 Show |

01 04 28 03 8B 01 01 00 00 15 00 FF 18 11 43 48 4B 49 4E 31 53 59 58 31 33 36 30 58 37 36 38 05 02 00 01 00 0D 00 00 00 00 00 05 50 00 A0 FF 00
00 00 05 03 00 01 01 00 0D 00 00 00 00 00 05 50 00 A0 FF 00 00 00 05 04 00 01 01 00 00 0D 00 00 00 00 00 05 50 00 A0 FF 00 00 00 0B 05 00 01 01
00 00 00 1A 00 00 00 00 00 05 50 00 A0 02 0F 31 33 36 30 78 31 36 30 5C 56 41 2E 67 69 66 0B 05 00 01 01 01 00 00 09 00 00 00 00 00 05 50 00 A0
0B 05 00 01 01 02 00 00 09 00 00 00 00 00 05 50 00 A0 0B 05 00 01 01 03 00 00 09 00 00 00 00 00 05 50 00 A0 0B 05 00 01 01 04 00 00 09 00 00 00
00 00 05 50 00 A0 05 02 00 02 00 0D 00 00 00 00 A0 05 50 01 75 FF 00 00 00 05 03 00 02 00 00 0D 00 00 00 00 00 02 58 00 A5 FF 00 00 00 05 04 00
02 00 00 00 0D 00 00 00 00 00 02 58 00 A5 FF 00 00 00 06 05 00 02 00 00 00 00 0F 00 00 4D 00 9B 01 30 02 06 56 41 20 31 32 33 07 03 00 02 01 00
0D 00 01 53 01 54 01 48 02 02 00 41 00 41 07 03 00 02 02 00 17 00 03 C9 01 54 01 CA 06 01 04 78 CD FF 00 00 00 00 02 02 00 41 00 41 07 04 00 02
03 01 00 0D 00 01 90 00 5A 01 46 02 02 00 41 00 41 07 04 00 02 03 02 00 17 00 04 38 00 5A 01 12 06 01 04 64 CD FF 01 00 00 00 02 02 00 41 00 41
06 03 00 02 05 00 21 00 02 8A 00 46 01 B8 06 01 02 2D 03 FF 00 00 00 00 02 0E 44 65 70 61 72 74 75 72 65 20 54 69 6D 65 07 03 00 02 06 00 1D 00
03 C5 00 46 01 B8 06 01 04 30 03 FF 00 00 00 00 02 05 8D 77 98 DE 65 F6 95 F4 00 3A 06 03 00 02 07 00 0D 00 04 A2 00 46 01 D8 02 04 30 36 30 30
06 03 00 02 08 00 23 00 02 8A 00 9B 01 B8 06 01 02 2D 03 FF 00 00 00 00 02 10 42 6F 61 72 64 69 6E 67 20 61 74 20 47 61 74 65 07 03 00 02 09 00
1B 00 03 F4 00 9B 01 B8 06 01 04 30 03 FF 00 00 00 00 02 04 76 7B 67 3A 95 E8 00 3A 05 03 00 02 0A 00 0D 00 04 BF 00 69 00 5A 00 41 14 00 00 00
06 04 00 02 0A 01 00 0A 00 00 1E 00 37 01 F3 02 01 33 05 02 00 0A 00 0D 00 00 00 02 4B 05 50 00 B4 FF 00 00 00 05 03 00 0A 01 00 0D 00 00 00 00
14 05 50 00 A0 FF 00 00 00 0B 04 00 0A 01 01 00 1D 00 00 00 00 00 05 50 00 A0 02 12 31 33 36 30 78 31 36 30 5C 56 41 5F 45 43 2E 67 69 66 05 04
00 0A 04 02 00 0D 00 00 00 00 50 05 50 00 64 FF 00 00 00 0B 05 00 0A 04 02 01 00 1D 00 00 00 00 00 05 50 00 64 02 12 31 33 36 30 78 31 30 30 5C
56 41 5F 45 43 2E 67 69 66 05 04 00 0A 05 02 00 0D 00 00 00 00 50 05 50 00 64 FF 00 00 00 0B 05 00 0A 05 02 01 00 1D 00 00 00 00 00 05 50 00 64
02 12 31 33 36 30 78 31 30 30 5C 56 41 5F 45 43 2E 67 69 66 05 04 00 0A 06 02 00 0D 00 00 00 00 50 05 50 00 64 FF 00 00 00 0B 05 00 0A 06 02 01
00 1D 00 00 00 00 00 05 50 00 64 02 12 31 33 36 30 78 31 30 30 5C 56 41 5F 45 43 2E 67 69 66 05 04 00 0A 07 02 00 0D 00 00 00 00 50 05 50 00 64
FF 00 00 00 0B 05 00 0A 07 02 01 00 1D 00 00 00 00 00 05 50 00 64 02 12 31 33 36 30 78 31 30 30 5C 56 41 5F 45 43 2E 67 69 66 01 01 00 00 01 03
	(FIDSxml)
id=0 fr=1001 len=925  PAGE 0 Del | PAGE 0 New Name "CheckinSingle.1" | WINDOW 0.1 New x=0 y=0 1360x310 |
WINDOW 0.1.1 New x=0 y=0 1360x160 | IMAGE 1 Put qf1360x160.gif | IMAGE 0.1.1.1 New 1360x160 Put qf1360x160.gif |
WINDOW 0.1.2 New x=0 y=160 1360x150 | FONT 5 New swissbc.ttf | TEXTSTYLE 1 New 5 size=130 |
WINDOW 0.1.2.1 New x=0 y=0 650x150 | UTEXT 0.1.2.1.1 New x=325 y=139  1 Put "QF123" |
WINDOW 0.2 New x=650 y=160 710x150 | WINDOW 0.2.1 New x=0 y=0 710x35 | WINDOW 0.2.1.1 New x=0 y=0 710x35 |
WINDOW 0.2.2 New x=0 y=35 710x55 | TEXTSTYLE 2 New 5 size=40 | WINDOW 0.2.2.1 New x=0 y=0 500x55 |
UTEXT 0.2.2.1.1 New x=3 y=42  2 Put "Departure Time:" | TEXTSTYLE 3 New 5 size=40 |
WINDOW 0.2.2.2 New x=500 y=0 90x55 | UTEXT 0.2.2.2.1 New x=45 y=42  3 Put "1200" |
WINDOW 0.2.3 New x=0 y=90 710x5 | WINDOW 0.2.3.1 New x=0 y=0 710x5 | WINDOW 0.2.4 New x=0 y=95 710x55 |
WINDOW 0.2.4.1 New x=0 y=0 500x55 | UTEXT 0.2.4.1.1 New x=3 y=42  2 Put "Boarding at Gate:" |
TEXTSTYLE 4 New 5 size=40 | WINDOW 0.2.4.2 New x=500 y=0 90x55 | UTEXT 0.2.4.2.1 New x=45 y=42  4 Put "5" |
WINDOW 0.3 New x=0 y=310 1360x458 | WINDOW 0.3.1 New x=0 y=0 1360x298 | WINDOW 0.3.1.1 New x=0 y=0 200x298 |
TEXTSTYLE 5 New 5 size=100 | WINDOW 0.3.1.2 New x=200 y=0 960x298 | UTEXT 0.3.1.2.1 New x=480 y=185  5 Put "Sydney Beirut" |
WINDOW 0.3.1.3 New x=1160 y=0 200x298 | WINDOW 0.3.2 New x=0 y=298 1360x160 | TEXTSTYLE 6 New 5 size=100 |
WINDOW 0.3.2.1 New x=0 y=0 1360x160 | UTEXT 0.3.2.1.1 New x=680 y=107  6 Put "Reserved For Class Logo" |
PAGE 0 Show |
00 03 E9 03 9D 01 00 00 01 01 01 00 00 13 00 FF 18 0F 43 68 65 63 6B 69 6E 53 69 6E 67 6C 65 2E 31 05 02 00 01 00 0D 00 00 00 00 00 05 50 01 36
FF 00 00 00 05 03 00 01 01 00 0D 00 00 00 00 00 05 50 00 A0 FF 00 00 00 0B 01 01 00 10 02 0E 71 66 31 33 36 30 78 31 36 30 2E 67 69 66 0B 04 00
01 01 01 00 19 00 00 00 00 00 05 50 00 A0 02 0E 71 66 31 33 36 30 78 31 36 30 2E 67 69 66 05 03 00 01 02 00 0D 00 00 00 00 A0 05 50 00 96 FF 00
00 00 04 01 05 00 0D 00 0B 73 77 69 73 73 62 63 2E 74 74 66 02 01 01 00 0A 00 01 05 82 CD FF 00 00 01 00 05 04 00 01 02 01 00 0D 00 00 00 00 00
02 8A 00 96 FF 00 00 00 07 05 00 01 02 01 01 00 13 00 01 45 00 8B 01 01 02 05 00 51 00 46 00 31 00 32 00 33 05 02 00 02 00 0D 00 02 8A 00 A0 02
C6 00 96 FF 00 00 00 05 03 00 02 01 00 0D 00 00 00 00 00 02 C6 00 23 FF 00 00 00 05 04 00 02 01 01 00 0D 00 00 00 00 00 02 C6 00 23 FF 00 00 00
05 03 00 02 02 00 0D 00 00 00 00 23 02 C6 00 37 FF 00 00 00 02 01 02 00 0A 00 01 05 28 03 FF 00 00 00 00 05 04 00 02 02 01 00 0D 00 00 00 00 00
01 F4 00 37 FF 00 00 00 07 05 00 02 02 01 01 00 27 00 00 03 00 2A 01 02 02 0F 00 44 00 65 00 70 00 61 00 72 00 74 00 75 00 72 00 65 00 20 00 54
00 69 00 6D 00 65 00 3A 02 01 03 00 0A 00 01 05 28 00 FF 00 00 01 00 05 04 00 02 02 02 00 0D 00 01 F4 00 00 00 5A 00 37 FF 00 00 00 07 05 00 02
02 02 01 00 11 00 00 2D 00 2A 01 03 02 04 00 31 00 32 00 30 00 30 05 03 00 02 03 00 0D 00 00 00 00 5A 02 C6 00 05 FF 00 00 00 05 04 00 02 03 01
00 0D 00 00 00 00 00 02 C6 00 05 FF 00 00 00 05 03 00 02 04 00 0D 00 00 00 00 5F 02 C6 00 37 FF 00 00 00 05 04 00 02 04 01 00 0D 00 00 00 00 00
01 F4 00 37 FF 00 00 00 07 05 00 02 04 01 01 00 2B 00 00 03 00 2A 01 02 02 11 00 42 00 6F 00 61 00 72 00 64 00 69 00 6E 00 67 00 20 00 61 00 74
00 20 00 47 00 61 00 74 00 65 00 3A 02 01 04 00 0A 00 01 05 28 FF 03 00 00 01 00 05 04 00 02 04 02 00 0D 00 01 F4 00 00 00 5A 00 37 03 00 00 00
07 05 00 02 04 02 01 00 0B 00 00 2D 00 2A 01 04 02 01 00 35 05 02 00 03 00 0D 00 00 00 01 36 05 50 01 CA FF 00 00 00 05 03 00 03 01 00 0D 00 00
00 00 00 05 50 01 2A FF 00 00 00 05 04 00 03 01 01 00 0D 00 00 00 00 00 00 C8 01 2A FF 00 00 00 02 01 05 00 0A 00 01 05 64 00 FF 00 00 01 00 05
04 00 03 01 02 00 0D 00 00 C8 00 00 03 C0 01 2A FF 00 00 00 07 05 00 03 01 02 01 00 23 00 01 E0 00 B9 01 05 02 0D 00 53 00 79 00 64 00 6E 00 65
00 79 00 20 00 42 00 65 00 69 00 72 00 75 00 74 05 04 00 03 01 03 00 0D 00 04 88 00 00 00 C8 01 2A FF 00 00 00 05 03 00 03 02 00 0D 00 00 00 01
2A 05 50 00 A0 CE 00 00 00 02 01 06 00 0A 00 01 05 64 FF CE 00 00 01 00 05 04 00 03 02 01 00 0D 00 00 00 00 00 05 50 00 A0 CE 00 00 00 07 05 00
03 02 01 01 00 37 00 02 A8 00 6B 01 06 02 17 00 52 00 65 00 73 00 65 00 72 00 76 00 65 00 64 00 20 00 46 00 6F 00 72 00 20 00 43 00 6C 00 61 00
73 00 73 00 20 00 4C 00 6F 00 67 00 6F 01 00 00 01 03

departures with wiping subfields and vscroll dest on line 1  flashing boarding on line 2
id=0 fr=1001 len=1213  PAGE 0 Del | PAGE 0 New Name "Departures.1" | WINDOW 0.1 New x=0 y=0 1360x768 |
WINDOW 0.1.1 New x=0 y=0 1360x60 | WINDOW 0.1.1.1 New x=0 y=0 150x60 | FONT 5 New swissbc.ttf |
TEXTSTYLE 1 New 5 size=55 | WINDOW 0.1.1.2 New x=150 y=0 1060x60 | UTEXT 0.1.1.2.1 New x=530 y=59  1 Put "DEPARTURES" |
TEXTSTYLE 2 New 5 size=34 | WINDOW 0.1.1.3 New x=1210 y=0 150x60 | UTEXT 0.1.1.3.1 New x=75 y=36  2 Put "15:00" |
WINDOW 0.1.2 New x=0 y=60 1360x40 | TEXTSTYLE 3 New 5 size=34 | WINDOW 0.1.2.1 New x=0 y=0 240x40 |
UTEXT 0.1.2.1.1 New x=10 y=36  3 Put "Airline" | WINDOW 0.1.2.2 New x=240 y=0 215x40 |
UTEXT 0.1.2.2.1 New x=3 y=36  3 Put "Flight" | WINDOW 0.1.2.3 New x=455 y=0 430x40 |
UTEXT 0.1.2.3.1 New x=3 y=36  3 Put "Destination" | WINDOW 0.1.2.4 New x=885 y=0 115x40 |
UTEXT 0.1.2.4.1 New x=3 y=36  3 Put "Schd" | WINDOW 0.1.2.5 New x=1000 y=0 115x40 |
UTEXT 0.1.2.5.1 New x=3 y=36  3 Put "Estm" | WINDOW 0.1.2.6 New x=1115 y=0 165x40 |
UTEXT 0.1.2.6.1 New x=3 y=36  3 Put "Status" | WINDOW 0.1.2.7 New x=1280 y=0 80x40 |
UTEXT 0.1.2.7.1 New x=40 y=36  2 Put "Gate" | WINDOW 0.1.3 New x=0 y=100 1360x1 |
WINDOW 0.1.3.1 New x=0 y=0 1360x1 | WINDOW 0.1.6 New x=0 y=101 1360x60 | VSCROLL 0.1.6.1 New x=0 y=0 455x60 |
IMAGE 1 Put qf230x60.gif | IMAGE 2 Put gt230x60.gif | IMAGE 0.1.6.1.0.0 New 230x60 Put qf230x60.gif |
IMAGE 0.1.6.1.1.0 New 230x60 Put gt230x60.gif | FONT 4 New swissm.ttf | TEXTSTYLE 4 New 4 size=44 |
UTEXT 0.1.6.1.0.1 New x=240 y=47  4 Put "QF 436" | UTEXT 0.1.6.1.1.1 New x=240 y=47  4 Put "GT 436" |
TEXTSTYLE 5 New 5 size=44 | VSCROLL 0.1.6.2 New x=455 y=0 430x60 | UTEXT 0.1.6.2.0.0 New x=3 y=47  5 Put "Sydney Beirut" |
UTEXT 0.1.6.2.1.0 New x=3 y=47  5 Put "Brisbane" | WINDOW 0.1.6.3 New x=885 y=0 115x60 |
UTEXT 0.1.6.3.1 New x=3 y=47  5 Put "1100" | TEXTSTYLE 6 New 5 size=44 | WINDOW 0.1.6.4 New x=1000 y=0 115x60 |
UTEXT 0.1.6.4.1 New x=3 y=47  6 Put "1107" | WINDOW 0.1.6.5 New x=1115 y=0 165x60 |
TEXTSTYLE 7 New 5 size=44 | WINDOW 0.1.6.6 New x=1280 y=0 80x60 | UTEXT 0.1.6.6.1 New x=40 y=47  7 Put "7" |
00 03 E9 04 BD 01 00 00 01 01 01 00 00 10 00 FF 18 0C 44 65 70 61 72 74 75 72 65 73 2E 31 05 02 00 01 00 0D 00 00 00 00 00 05 50 03 00 FF 00 00
00 05 03 00 01 01 00 0D 00 00 00 00 00 05 50 00 3C E9 00 00 00 05 04 00 01 01 01 00 0D 00 00 00 00 00 00 96 00 3C E9 00 00 00 04 01 05 00 0D 00
0B 73 77 69 73 73 62 63 2E 74 74 66 02 01 01 00 0A 00 01 05 37 00 E9 00 00 01 00 05 04 00 01 01 02 00 0D 00 00 96 00 00 04 24 00 3C E9 00 00 00
07 05 00 01 01 02 01 00 1D 00 02 12 00 3B 01 01 02 0A 00 44 00 45 00 50 00 41 00 52 00 54 00 55 00 52 00 45 00 53 02 01 02 00 0A 00 01 05 22 00
E9 00 00 01 00 05 04 00 01 01 03 00 0D 00 04 BA 00 00 00 96 00 3C E9 00 00 00 07 05 00 01 01 03 01 00 13 00 00 4B 00 24 01 02 02 05 00 31 00 35
00 3A 00 30 00 30 05 03 00 01 02 00 0D 00 00 00 00 3C 05 50 00 28 E9 00 00 00 02 01 03 00 0A 00 01 05 22 00 E9 00 00 00 00 05 04 00 01 02 01 00
0D 00 00 00 00 00 00 F0 00 28 E9 00 00 00 07 05 00 01 02 01 01 00 17 00 00 0A 00 24 01 03 02 07 00 41 00 69 00 72 00 6C 00 69 00 6E 00 65 05 04
00 01 02 02 00 0D 00 00 F0 00 00 00 D7 00 28 E9 00 00 00 07 05 00 01 02 02 01 00 15 00 00 03 00 24 01 03 02 06 00 46 00 6C 00 69 00 67 00 68 00
74 05 04 00 01 02 03 00 0D 00 01 C7 00 00 01 AE 00 28 E9 00 00 00 07 05 00 01 02 03 01 00 1F 00 00 03 00 24 01 03 02 0B 00 44 00 65 00 73 00 74
00 69 00 6E 00 61 00 74 00 69 00 6F 00 6E 05 04 00 01 02 04 00 0D 00 03 75 00 00 00 73 00 28 E9 00 00 00 07 05 00 01 02 04 01 00 11 00 00 03 00
24 01 03 02 04 00 53 00 63 00 68 00 64 05 04 00 01 02 05 00 0D 00 03 E8 00 00 00 73 00 28 E9 00 00 00 07 05 00 01 02 05 01 00 11 00 00 03 00 24
01 03 02 04 00 45 00 73 00 74 00 6D 05 04 00 01 02 06 00 0D 00 04 5B 00 00 00 A5 00 28 E9 00 00 00 07 05 00 01 02 06 01 00 15 00 00 03 00 24 01
03 02 06 00 53 00 74 00 61 00 74 00 75 00 73 05 04 00 01 02 07 00 0D 00 05 00 00 00 00 50 00 28 E9 00 00 00 07 05 00 01 02 07 01 00 11 00 00 28
00 24 01 02 02 04 00 47 00 61 00 74 00 65 05 03 00 01 03 00 0D 00 00 00 00 64 05 50 00 01 03 00 00 00 05 04 00 01 03 01 00 0D 00 00 00 00 00 05
50 00 01 03 00 00 00 05 03 00 01 06 00 0D 00 00 00 00 65 05 50 00 3C FF 00 00 00 0D 04 00 01 06 01 00 0C 00 00 00 00 00 01 C7 00 3C 02 FF 09 0B
01 01 00 0E 02 0C 71 66 32 33 30 78 36 30 2E 67 69 66 0B 01 02 00 0E 02 0C 67 74 32 33 30 78 36 30 2E 67 69 66 0B 06 00 01 06 01 00 00 00 17 00
00 00 00 00 00 E6 00 3C 02 0C 71 66 32 33 30 78 36 30 2E 67 69 66 0B 06 00 01 06 01 01 00 00 17 00 00 00 00 00 00 E6 00 3C 02 0C 67 74 32 33 30
78 36 30 2E 67 69 66 04 01 04 00 0C 00 0A 73 77 69 73 73 6D 2E 74 74 66 02 01 04 00 0A 00 01 04 2C CD FF 00 00 00 00 07 06 00 01 06 01 00 01 00
15 00 00 F0 00 2F 01 04 02 06 00 51 00 46 00 20 00 34 00 33 00 36 07 06 00 01 06 01 01 01 00 15 00 00 F0 00 2F 01 04 02 06 00 47 00 54 00 20 00
34 00 33 00 36 02 01 05 00 0A 00 01 05 2C 00 FF 00 00 00 00 0D 04 00 01 06 02 00 0C 00 01 C7 00 00 01 AE 00 3C 02 FF 00 07 06 00 01 06 02 00 00
00 23 00 00 03 00 2F 01 05 02 0D 00 53 00 79 00 64 00 6E 00 65 00 79 00 20 00 42 00 65 00 69 00 72 00 75 00 74 07 06 00 01 06 02 01 00 00 19 00
00 03 00 2F 01 05 02 08 00 42 00 72 00 69 00 73 00 62 00 61 00 6E 00 65 05 04 00 01 06 03 00 0D 00 03 75 00 00 00 73 00 3C FF 00 00 00 07 05 00
01 06 03 01 00 11 00 00 03 00 2F 01 05 02 04 00 31 00 31 00 30 00 30 02 01 06 00 0A 00 01 05 2C 03 FF 00 00 00 00 05 04 00 01 06 04 00 0D 00 03
E8 00 00 00 73 00 3C FF 00 00 00 07 05 00 01 06 04 01 00 11 00 00 03 00 2F 01 06 02 04 00 31 00 31 00 30 00 37 05 04 00 01 06 05 00 0D 00 04 5B
00 00 00 A5 00 3C FF 00 00 00 02 01 07 00 0A 00 01 05 2C FF 03 00 00 01 00 05 04 00 01 06 06 00 0D 00 05 00 00 00 00 50 00 3C 03 00 00 00 07 05
00 01 06 06 01 00 0B 00 00 28 00 2F 01 07 02 01 00 37

id=0 fr=1001 len=737  WINDOW 0.1.7 New x=0 y=161 1360x2 | WINDOW 0.1.7.1 New x=0 y=0 1360x2 |
WINDOW 0.1.11 New x=0 y=163 1360x60 | IMAGE 0.1.11.26 New 230x60 Put qf230x60.gif |
WINDOW 0.1.11.27 New x=230 y=0 225x60 | UTEXT 0.1.11.27.1 New x=10 y=47  4 Put "QF 123" |
TEXTSTYLE 8 New 5 size=44 | WINDOW 0.1.11.2 New x=455 y=0 430x60 | UTEXT 0.1.11.2.1 New x=3 y=47  8 Put "Sydney Beirut" |
WINDOW 0.1.11.3 New x=885 y=0 115x60 | UTEXT 0.1.11.3.1 New x=3 y=47  8 Put "1200" |
TEXTSTYLE 9 New 5 size=44 | WINDOW 0.1.11.4 New x=1000 y=0 115x60 | UTEXT 0.1.11.4.1 New x=3 y=47  9 Put "1207" |
TEXTSTYLE 10 New 5 size=34 | WINDOW 0.1.11.5 New x=1115 y=0 165x60 | PAGE 0 SetMap i=5 rgb=255 0 0 |
PAGE 0 SetBlink i=5 rgb=0 0 0 | UTEXT 0.1.11.5.1 New x=82 y=44  10 Put "Boarding" |
WINDOW 0.1.11.6 New x=1280 y=0 80x60 | UTEXT 0.1.11.6.1 New x=40 y=47  7 Put "5" |
WINDOW 0.1.12 New x=0 y=223 1360x60 | TEXTSTYLE 11 New 5 size=34 | WINDOW 0.1.12.1 New x=0 y=0 1360x60 |
UTEXT 0.1.12.1.1 New x=3 y=36  11 Put "Would passenger Graeme Tunbridge please report to the gate lounge immediately." |
WINDOW 0.1.13 New x=0 y=283 1360x2 | WINDOW 0.1.13.1 New x=0 y=0 1360x2 | PAGE 0 Show |
00 03 E9 02 E1 05 03 00 01 07 00 0D 00 00 00 00 A1 05 50 00 02 68 00 00 00 05 04 00 01 07 01 00 0D 00 00 00 00 00 05 50 00 02 68 00 00 00 05 03
00 01 0B 00 0D 00 00 00 00 A3 05 50 00 3C FF 00 00 00 0B 04 00 01 0B 1A 00 17 00 00 00 00 00 00 E6 00 3C 02 0C 71 66 32 33 30 78 36 30 2E 67 69
66 05 04 00 01 0B 1B 00 0D 00 00 E6 00 00 00 E1 00 3C FF 00 00 00 07 05 00 01 0B 1B 01 00 15 00 00 0A 00 2F 01 04 02 06 00 51 00 46 00 20 00 31
00 32 00 33 02 01 08 00 0A 00 01 05 2C 00 BB 00 00 00 00 05 04 00 01 0B 02 00 0D 00 01 C7 00 00 01 AE 00 3C BB 00 00 00 07 05 00 01 0B 02 01 00
23 00 00 03 00 2F 01 08 02 0D 00 53 00 79 00 64 00 6E 00 65 00 79 00 20 00 42 00 65 00 69 00 72 00 75 00 74 05 04 00 01 0B 03 00 0D 00 03 75 00
00 00 73 00 3C BB 00 00 00 07 05 00 01 0B 03 01 00 11 00 00 03 00 2F 01 08 02 04 00 31 00 32 00 30 00 30 02 01 09 00 0A 00 01 05 2C 03 BB 00 00
00 00 05 04 00 01 0B 04 00 0D 00 03 E8 00 00 00 73 00 3C BB 00 00 00 07 05 00 01 0B 04 01 00 11 00 00 03 00 2F 01 09 02 04 00 31 00 32 00 30 00
37 02 01 0A 00 0A 00 01 05 22 00 0F 00 00 01 00 05 04 00 01 0B 05 00 0D 00 04 5B 00 00 00 A5 00 3C 05 00 00 00 01 00 00 05 04 05 FF 00 00 01 00
00 05 05 05 00 00 00 07 05 00 01 0B 05 01 00 19 00 00 52 00 2C 01 0A 02 08 00 42 00 6F 00 61 00 72 00 64 00 69 00 6E 00 67 05 04 00 01 0B 06 00
0D 00 05 00 00 00 00 50 00 3C 03 00 00 00 07 05 00 01 0B 06 01 00 0B 00 00 28 00 2F 01 07 02 01 00 35 05 03 00 01 0C 00 0D 00 00 00 00 DF 05 50
00 3C BB 00 00 00 02 01 0B 00 0A 00 01 05 22 00 BB 00 00 00 00 05 04 00 01 0C 01 00 0D 00 00 00 00 00 05 50 00 3C BB 00 00 00 07 05 00 01 0C 01
01 00 A5 00 00 03 00 24 01 0B 02 4E 00 57 00 6F 00 75 00 6C 00 64 00 20 00 70 00 61 00 73 00 73 00 65 00 6E 00 67 00 65 00 72 00 20 00 47 00 72
00 61 00 65 00 6D 00 65 00 20 00 54 00 75 00 6E 00 62 00 72 00 69 00 64 00 67 00 65 00 20 00 70 00 6C 00 65 00 61 00 73 00 65 00 20 00 72 00 65
00 70 00 6F 00 72 00 74 00 20 00 74 00 6F 00 20 00 74 00 68 00 65 00 20 00 67 00 61 00 74 00 65 00 20 00 6C 00 6F 00 75 00 6E 00 67 00 65 00 20
00 69 00 6D 00 6D 00 65 00 64 00 69 00 61 00 74 00 65 00 6C 00 79 00 2E 05 03 00 01 0D 00 0D 00 00 00 01 1B 05 50 00 02 68 00 00 00 05 04 00 01
0D 01 00 0D 00 00 00 00 00 05 50 00 02 68 00 00 00 01 00 00 01 03

id=0 fr=1001 len=120  TEXTSTYLE 5 New 5 size=96 col=0/255 cn | WINDOW 0.1.3.1 New x=0 y=0 1360x288 |
UTFTEXT 0.1.3.1.1 New x=680 y=123  0 PutAttr 5 sz=115 Put 0 0 "Brisbane" | UTEXT 0.1.3.1.2 New x=680 y=244  5 Put "London Paris" |
PAGE 0 Update |
00 03 E9 00 78 02 01 05 00 0A 00 01 05 60 00 FF 00 00 01 00 05 04 00 01 03 01 00 0D 00 00 00 00 00 05 50 01 20 FF 00 00
00 10 05 00 01 03 01 01 00 1C 00 02 A8 00 7B 00 06 01 05 73 00 FF 00 00 01 00 02 00 00 08 42 72 69 73 62 61 6E 65 07 05
00 01 03 01 02 00 21 00 02 A8 00 F4 01 05 02 0C 00 4C 00 6F 00 6E 00 64 00 6F 00 6E 00 20 00 50 00 61 00 72 00 69 00 73
01 00 00 01 1A

id=0 fr=1001 len=5  PAGE 0 Show |
id=0 fr=1001 len=5  PAGE 0 Show |
id=0 fr=1001 len=5  PAGE 0 Show |
id=0 fr=1001 len=39  WINDOW 0.1 New x=0 y=0 1360x768 | WINDOW 0.1.7 New x=0 y=157 1360x62 |
id=0 fr=1001 len=22  FILE 0 New gt230x60.gif size=2712 |
id=0 fr=1001 len=1109  FILE 0 Put size=1100 |
id=0 fr=1001 len=1109  FILE 0 Put size=1100 |
id=0 fr=1001 len=521  FILE 0 Put size=512 |
id=0 fr=1001 len=119  IMAGE 0.1.7.11 New 230x60 Put gt230x60.gif | VSCROLL 0.1.7.12 New x=1130 y=1 230x60 n=2 15 0 |
UTFTEXT 0.1.7.12.0.0 New x=10 y=42 0 Put 0 utf8 "Hello UDC" | IMAGE 0.1.7.12.1.0 New 230x60 Put gt230x60.gif |
PAGE 0 Show |   this works. ie SetGIF/aPut is redundant

id=0 fr=1001 len=153  WINDOW 0.1 New x=0 y=0 1360x768 | WINDOW 0.1.7 New x=0 y=157 1360x62 |
IMAGE 0.1.7.11 New 230x60 Put gt230x60.gif | VSCROLL 0.1.7.12 New x=1130 y=1 230x60 n=2 15 0 |
UTFTEXT 0.1.7.12.0.0 New x=10 y=42 0 Put 0 utf8 "Hello UDC" | IMAGE 0.1.7.12.1.0 New 230x60 Put gt230x60.gif |
id=0 fr=1001 len=22  FILE 0 New gt230x60.gif size=2712 |
id=0 fr=1001 len=1109  FILE 0 Put size=1100 |
id=0 fr=1001 len=1109  FILE 0 Put size=1100 |
id=0 fr=1001 len=521  FILE 0 Put size=512 |
id=0 fr=1001 len=5  PAGE 0 Show |


id=0 fr=0 len=10  DISPLAY 0 VsRate spd=0 del=420 |
00 00 00 00 0A 00 01 00 00 05 1C 00 00 01 A4

id=0 fr=0 len=44  DISPLAY 0 Time 14:55 Epoch 635352901 Sip 192.168.45.250 : 9200 SetHosts 1/1/192.168.45.250

}

end.
