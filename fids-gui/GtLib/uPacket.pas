unit uPacket;

interface

uses uGT, Windows, Messages, SyncObjs, SysUtils, Contnrs, uGlobalDefs, IdGlobal;

type
	aPktType = (pkNone, pkConnectTo, pkDisconnect, pkRequestServerIP,
	  pkAnnounceServer, pkPassThrough);
	aLink = (alNone, alLocal, alPipe, alTCP, alUDP, alSerial);
	aArray = array [0 .. 3 { variable } ] of byte;
	apArray = ^aArray;

	aPacket = record
	public
		pkSize: card; // total size of message including pkSize field
		function ToString(): string;
		function ToBytes(): TBytes;
		procedure FreePkt();
		case boolean of // variant part must be last
			true:
				( // header part separately addressed
				  pkType: aPktType; // bit 7 => compressed data
				  pkSrcID: byte;
				  pkFlags: int16; // comes for free as padding
				  pkData: array [0 .. 3 { variable } ] of byte
				);
			false:
				( // whole thing header and all if there is one
				  pkRawData: array [0 .. 3 { variable } ] of byte
				)
	end;

	apPacket = ^aPacket;

	aEventHandler = procedure of object;
	apEventHandler = ^aEventHandler;

	cLockedQueue = class;
	aInputHandler = procedure(q: cLockedQueue; var wm: TMessage) of object;

	cLockedQueue = class(TQueue)
		constructor Create(wh: HWND; winMesgNo: int); overload;
		constructor Create(ih: aInputHandler); overload;
		destructor Destroy; override;
	private
		oLock: TCriticalSection;
		mhWindow: HWND;
		mWinMesgNo: int;
		mReader: aInputHandler;
	public
		procedure WMhandler(var wm: TMessage); // Main thread only here
		procedure Post(pm: apPacket; link: aLink);
		procedure PostPointer(pm: pointer); // untyped overload
		function Read(): apPacket;
		function ReadPointer(): pointer; // untyped overload
		function Count(): int;

	end;

const
	MagicFlags = $4754;
	SizeOfPktHdr = 8;
	// SizeOf( aPacket without pkData );    SizeOf( pm ^ ) - SizeOf( pm.pkData )
	WM_Queue = WM_USER + 847;

function PktBytes(const str: string; mType: aPktType = pkPassThrough): TIdBytes;
function NewPkt(const str: string): apPacket; overload;
function NewPkt(const buf: array of byte): apPacket; overload;
function NewPkt(s: int): apPacket; overload;
function NewRawPkt(const buf: array of byte): apPacket;
// for unformatted incoming
function CopyBytes(const buf: array of byte): aBytes;
function CopyString(const str: string): string;
procedure Add(var buf: aBytes; b: byte);

implementation

uses
	Classes, Dialogs;

function NewPkt(const str: string): apPacket;

var
	s: cardinal;
	pm: apPacket;
begin
	s := SizeOfPktHdr + ByteLength(str);
	GetMem(pm, s);
	pm.pkType := pkNone;
	pm.pkSrcID := 0;
	pm.pkFlags := MagicFlags; // zero id, type, and 'GT' flags
	// for i := Low( pm.pkFlags ) to High( pm.pkFlags ) do  pm.pkFlags[ i ] := 0;
	pm.pkSize := s;
	Move(str[1], pm.pkData, ByteLength(str)); // mem copy string -> data buffer
	result := pm;
end;

function NewPkt(s: int): apPacket; overload;

begin
	GetMem(result, s + SizeOf(aPacket));
	result.pkSize := s;
end;

function NewPkt(const buf: array of byte): apPacket; overload;

var
	s: int;
begin
	s := Length(buf);
	GetMem(result, s);
	Move(buf[0], result.pkSize, s); // mem copy string -> data buffer
end;

function NewRawPkt(const buf: array of byte): apPacket;

begin
	GetMem(result, Length(buf) + SizeOf(aPacket)); // slight overkill
	result.pkSize := Length(buf);
	Move(buf[0], result.pkRawData[0], Length(buf)); // mem copy buffer -> packet
end;

function PktBytes(const str: string; mType: aPktType = pkPassThrough): TIdBytes;

var
	s: int;
	pm: apPacket;
begin
	s := SizeOfPktHdr + ByteLength(str);
	SetLength(result, s);
	pm := @result[0];
	pm.pkType := mType;
	pm.pkSrcID := 0;
	pm.pkFlags := MagicFlags; // zero id, type, and 'GT' flags
	pm.pkSize := s;
	Move(str[1], pm.pkData, ByteLength(str)); // mem copy string -> data buffer
end;

function CopyBytes(const buf: array of byte): aBytes;

begin
	SetLength(result, Length(buf));
	Move(buf, result, Length(buf)); // result := Copy( buf, 0, Length( buf ) - 1
end;

function CopyString(const str: string): string;

begin
	SetLength(result, Length(str));
	result := Copy(str, 1, Length(str));
end;

procedure Add(var buf: aBytes; b: byte);

var
	len: int;
begin
	len := Length(buf);
	SetLength(buf, len + 1);
	buf[len] := b;
end;

function aPacket.ToString(): string;

var
	s: cardinal;
begin
	s := self.pkSize - SizeOfPktHdr;
	s := s and $FFFFFFFE;
	SetLength(result, s div SizeOf(Char));
	Move(self.pkData, result[1], s); // mem copy data buffer -> string
end;

function aPacket.ToBytes(): TBytes;

begin
	SetLength(result, self.pkSize);
	Move(self, result[0], self.pkSize); // mem copy
end;

procedure aPacket.FreePkt();

begin
	FreeMem(@self);
end;

constructor cLockedQueue.Create(wh: HWND; winMesgNo: int);

begin
	mhWindow := wh;
	mWinMesgNo := winMesgNo;
	oLock := TCriticalSection.Create;
	inherited Create;
end;

constructor cLockedQueue.Create(ih: aInputHandler); // overload;

begin
	mReader := ih;
	mhWindow := AllocateHWnd(WMhandler); // dummy window so I can get messages
	mWinMesgNo := WM_Queue;
	oLock := TCriticalSection.Create;
	inherited Create;
end;

destructor cLockedQueue.Destroy;

begin
	if self <> nil then
	begin
		oLock.Free;
		DeallocateHWnd(mhWindow);
		inherited Destroy;
	end;
end;

procedure cLockedQueue.WMhandler(var wm: TMessage); // Main thread only here

begin
	wm.result := 0;
	if wm.Msg = WM_Queue then
	begin
		if Assigned(mReader) then
			mReader(self, wm); // and ( wm.Msg = WM_Queue )
	end
	else
		wm.result := DefWindowProc(mhWindow, wm.Msg, wm.wParam, wm.lParam);
end;

procedure cLockedQueue.Post(pm: apPacket; link: aLink);
// typically reader thread posting incoming to main thread

begin
	oLock.Acquire;
	Push(pm);
	oLock.Release;
	if pm.pkSize > 1000 then
	begin
		mWinMesgNo := mWinMesgNo;
	end;
	if mhWindow <> 0 then
	begin
		if not PostMessage(mhWindow, mWinMesgNo, Ord(link), 0) then
		// signal main thread to empty the buffer
			ShowMessage('PostMessage fail ' + IntToStr(GetLastError) + ' : ' +
			  IntToStr(mhWindow));
	end;
end;

function cLockedQueue.Read(): apPacket;
// typically main thread reading incoming

begin
	// MonitorEnter( self );
	oLock.Acquire;
	result := Pop();
	// MonitorExit( self );
	oLock.Release;
end;

function cLockedQueue.Count(): int;

begin
	result := inherited Count();
end;

procedure cLockedQueue.PostPointer(pm: pointer);

begin
	Post(apPacket(pm), aLink(0));
end;

function cLockedQueue.ReadPointer(): pointer;

begin
	result := Read;
end;

end.
