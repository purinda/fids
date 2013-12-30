unit uPacket;

interface

uses	uGT, Windows, Messages, SyncObjs, SysUtils, Contnrs;

type
	aPktType = ( pkNone, pkConnectTo, pkDisconnect, pkRequestServerIP, pkAnnounceServer, pkPassThrough );
	aLink = ( alNone, alLocal, alPipe, alTCP, alUDP, alSerial );
    aArray = array[ 0..3 {variable} ] of byte;
    apArray = ^ aArray;

	aPacket = record
    	public
            pkSize : card;   // total size of message including pkSize field
            function    ToString() : string;
            function    ToBytes() : TBytes;
            procedure	FreePkt();
            case boolean of      // variant part must be last
            	true : (
                    pkType : aPktType;   // bit 7 => compressed data
                    pkSrcID : byte;
                    pkFlags : int16;  // comes for free as padding
                    pkData : array[ 0..3 { variable } ] of byte
                    );
                false : (
                	pkRawData : array[ 0..3 { variable } ] of byte
                	)
                end;
	apPacket = ^ aPacket;

    aEventHandler = procedure of object;
    apEventHandler = ^ aEventHandler;

    cLockedQueue = class;
    aInputHandler = procedure ( q : cLockedQueue; var wm : TMessage ) of object;

    cLockedQueue = class( TQueue )
        constructor Create( wh : HWND; winMesgNo : int );   overload;
        constructor Create( ih : aInputHandler );   overload;
        destructor  Destroy;   override;
        private
            // mLock : TCriticalSection;   use monitor
			mhWindow : HWND;
			mWinMesgNo : int;
            mReader : aInputHandler;
			procedure WMQueue( var wm : TMessage );  // Main thread only here
        public
			procedure Post( pm : apPacket; link : aLink );
			procedure PostPointer( pm : pointer );    // untyped overload
			function  Read() : apPacket;
			function  ReadPointer() : pointer;        // untyped overload
			function  Count() : int;

	    end;


const
	MagicFlags = $4754;
	SizeOfPktHdr = 8; // SizeOf( aPacket without pkData );    SizeOf( pm ^ ) - SizeOf( pm.pkData )
    WM_Queue = WM_USER + 847;


function    PktBytes( const str : string; mType : aPktType = pkPassThrough ) : TBytes;
function	NewPkt( const str : string ) : apPacket;  overload;
function  	NewPkt( const buf : TBytes ) : apPacket;  overload;
function    NewPkt( s : int ) : apPacket;  overload;
function    NewRawPkt( const buf : TBytes ) : apPacket;  // for unformatted incoming
function    CopyBytes( const buf : TBytes ) : TBytes;
function    CopyString( const str : string ) : string;
procedure   Add( var buf : TBytes; b : byte );


implementation

uses
	Classes;


function  NewPkt( const str : string ) : apPacket;

	var
		s : cardinal;
		pm : apPacket;
	begin
	s := SizeOfPktHdr + ByteLength( str );
	GetMem( pm, s );
	pm.pkType := pkNone;
	pm.pkSrcID := 0;
	pm.pkFlags := MagicFlags;  // zero id, type, and 'GT' flags
	// for i := Low( pm.pkFlags ) to High( pm.pkFlags ) do  pm.pkFlags[ i ] := 0;
	pm.pkSize := s;
	Move( str[ 1 ], pm.pkData, ByteLength( str ) );  // mem copy string -> data buffer
	result := pm;
	end;


function    NewPkt( s : int ) : apPacket;  overload;

	begin
	GetMem( result, s );
	result.pkSize := s;
	end;


function  NewPkt( const buf : TBytes ) : apPacket;  overload;

	var
		s : int;
	begin
    s := Length( buf );
	GetMem( result, s );
	Move( buf[ 0 ], result.pkSize, s );  // mem copy string -> data buffer
	end;


function  NewRawPkt( const buf : TBytes ) : apPacket;

	begin
	GetMem( result, Length( buf ) + SizeOf( aPacket ) );   // slight overkill
    result.pkSize := Length( buf );
	Move( buf[ 0 ], result.pkRawData[ 0 ], Length( buf ) );  // mem copy buffer -> packet
	end;


function  PktBytes( const str : string; mType : aPktType = pkPassThrough ) : TBytes;

	var
		s : int;
		pm : apPacket;
	begin
	s := SizeOfPktHdr + ByteLength( str );
	SetLength( result, s );
	pm := @result[ 0 ];
	pm.pkType := mType;
	pm.pkSrcID := 0;
	pm.pkFlags := MagicFlags;  // zero id, type, and 'GT' flags
	pm.pkSize := s;
	Move( str[ 1 ], pm.pkData, ByteLength( str ) );  // mem copy string -> data buffer
	end;


function    CopyBytes( const buf : TBytes ) : TBytes;

	begin
    SetLength( result, Length( buf ) );
    result := Copy( buf, 0, Length( buf ) - 1 );
    end;


function    CopyString( const str : string ) : string;

	begin
    SetLength( result, Length( str ) );
    result := Copy( str, 1, Length( str ) );
    end;


procedure   Add( var buf : TBytes; b : byte );

    var
    	len : int;
	begin
    len := Length( buf );
    SetLength( buf, len + 1 );
    buf[ len ] := b;
    end;


function    aPacket.ToString() : string;

	var
		s : cardinal;
	begin
	s := self.pkSize - SizeOfPktHdr;
    s := s and $FFFFFFFE;
    SetLength( result, s div SizeOf( Char ) );
	Move( self.pkData, result[ 1 ], s );  // mem copy data buffer -> string
    end;


function    aPacket.ToBytes() : TBytes;

	begin
    SetLength( result, self.pkSize );
	Move( self, result[ 0 ], self.pkSize );  // mem copy
    end;


procedure	aPacket.FreePkt();

	begin
	FreeMem( @ self );
	end;


constructor cLockedQueue.Create( wh : HWND; winMesgNo : int );

	begin
    mhWindow := wh;
    mWinMesgNo := winMesgNo;
    // mLock := TCriticalSection.Create;
    inherited Create;
    end;


constructor cLockedQueue.Create( ih : aInputHandler );   // overload;

	begin
    mReader := ih;
    mhWindow := AllocateHWnd( WMQueue ); //  dummy window so I can get messages
    mWinMesgNo := WM_Queue;
    // mLock := TCriticalSection.Create;
    inherited Create;
    end;


destructor  cLockedQueue.Destroy;

	begin
    if self <> nil then  begin
    	// mLock.Free;
    	inherited  Destroy;
	    end;
    end;




procedure	cLockedQueue.WMQueue( var wm : TMessage );  // Main thread only here

	begin
    if Assigned( mReader ) and ( wm.Msg = WM_Queue ) then  mReader( self, wm );
    end;



procedure  cLockedQueue.Post( pm : apPacket; link : aLink ); // typically reader thread posting incoming to main thread

	begin
    MonitorEnter( self );
	// mLock.Acquire;
	Push( pm );
    MonitorExit( self );
	// mLock.Release;
    if mhWindow <> 0 then  begin
		PostMessage( mhWindow, mWinMesgNo, Ord( link ), 0 ); // signal main thread to empty the buffer
	    end;
	end;


function  cLockedQueue.Read() : apPacket;   // typically main thread reading incoming

	begin
    MonitorEnter( self );
	// mLock.Acquire;
	result := Pop();
    MonitorExit( self );
	// mLock.Release;
	end;


function  cLockedQueue.Count() : int;

	begin
	result := inherited Count();
	end;


procedure  cLockedQueue.PostPointer( pm : pointer );

	begin
    Post( apPacket( pm ), aLink( 0 ) );
    end;


function   cLockedQueue.ReadPointer() : pointer;

	begin
    result := Read;
    end;


end.
