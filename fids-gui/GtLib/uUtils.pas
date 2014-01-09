unit uUtils;

interface

uses
	uGT, Classes, TypInfo, Types, SysUtils;  //  Graphics, Controls,

const
	ListSep = ',';   // formatter content list separator
	StringDelim = '"';

type
	aStreamDirection = ( sdNone, sdRead, sdWrite );
    cCharStream = class // ( TStream )
        public
        	//constructor Create( name : string );   abstract;
            //destructor	Destroy;   abstract;
            function    Read( var ch : char ) : boolean;   virtual;  abstract;// override;   // next char
            function	Write( ch : char ) : boolean;   virtual;  abstract;// override;
    	end;

	cTextFileStream = class( cCharStream ) // ( TStream )
        public
        	constructor Create( name : string );
            destructor	Destroy;  override;
            function    Read( var ch : char ) : boolean;  override;   // next char
            function	Write( ch : char ) : boolean;   override;
        private
        	mTF : textfile;
            mBuf : string;
    		oFile : TFileStream;
    		//mX: Integer;
        	mEncoding : ( enNone, enUTF8, enUTF16 );   // type of read file
            mDirection : aStreamDirection;
    		mName: string;
    	end;

	cStringStream = class ( cCharStream )
        public
        	constructor Create( source : string );
            destructor	Destroy;  override;
            function    Read( var ch : char ) : boolean;  override;   // next char
            function	Write( ch : char ) : boolean;   override;
        private
            mBuf : string;
    		mX	: Integer;
            //mDirection : aStreamDirection;
    	end;
     aKeyState = ( ksShift, ksControl, ksAlt );
     asKeyState = set of aKeyState;



function	Min( a, b : int ) : int;   // use System.Math
function	Max( a, b : int ) : int;
function    CountBits( val : card ) : card;
function	RectWH( ALeft, ATop, w, h : Integer): TRect;

procedure  LeaveOnly( leave : string; var s : string );
procedure  StripStart( var s : string );
procedure  StripEnd( var s : string );
procedure  SkipSpace( const s : string; var index : integer );
function   WithoutSpaces( const s : string ) : string;
function   GetString( const s : string; var index : integer ) : string;  // mod 5Oct2013
function   GetListItem( const s : string; var index : integer ) : string;
function   Slice( const src : string; index : integer; count : integer ) : string;
procedure  WindowsPath( var s : string );
function   PosN( const substr, src : string; limit : integer ) : int;
function   IsAlpha( c : char ) : boolean;
function   IsAlphaN( c : char ) : boolean;   inline;
function   IsDecimal( c : char ) : boolean;  overload;
function   IsLowerCase( c : char ) : boolean;
function   IsUpperCase( c : char ) : boolean;
function   IsAlphaNumeric( c : char ) : boolean;
function   StrIndex( s : string; x : int = -1 ) : char;
function   Gap( const s : string ) : string;
function   GetAlphas( const s : string; var index : integer ) : string;
function	GetHexNibble( ch : char; out val : int ) : boolean;
function	GetHexByte( const s : string; var x : int ) : byte;
function   GetHex( const s : string; var index  : integer ) : int;  //  F00BAA or , $F00BAA
function   GetInt( const s : string; var index : integer ) : integer;
function   GetBin( const s : string; var index  : integer ) : int;  //  F00BAA or , $F00BAA
function   Decimal( const s : string; var index : cardinal ) : cardinal;  overload;
function   Decimal( const s : string; var index : int ) : int;  overload;
function	StrX( const s : string; const x : int ) : char;  inline;  // safe string indexing
function	PosLast( ch : char;  const s : string ) : int;
function	IntToStrN( val, n : int ) : string;   // eg 007
function	UTF8toChars( fs : TStream; out ch : char ) : boolean;   // UTF8 convertion on byte stream

function	Cash( amt : currency; show0 : boolean = false ) : string;
function	TryCashToCurr( txt : string; var amt : currency ) : boolean;
function	ToDate( date : TDate ) : string;  overload;  // YYYYMMDD format eg 20130930 so string sort = date sort
function	ToDate( date : string ) : TDate;  overload;

function   InList( const item, list : string ) : boolean;
function   ListIndex( const item, list : string ) : integer;
function   ListItem( const list : TStringList; index : int ) : string;    overload;
function   ListItem( const list : string; index : int ) : string;         overload;
function   BuildParamsL( const params : string; var index : int; sep : char = ListSep ) : TStringList;
function   ListToStr( const list : TStringList ) : string;   overload;
function   ListToStr( const vals : array of int ) : string;   overload;
function   ListsEqual( const listA, listB : TStrings ) : boolean;
function   IncludeInList( const item, list : string ) : string;
function   ExcludeFromList( const item, list : string ) : string;
function   StrToList( const s : string ) : TStringList;
function   GetLine( const s : string; var index : integer ) : string;
function   LineCount( const s : string ) : int;
function    NoGapLower( s : string; lower : boolean = true ) : string;
function   UnCamel( const n : string; gapChar : char = ' ' ) : string;
function   EnumToStr( val : Integer; pEnumTypePtr : PTypeInfo; unCaml : boolean = true ) : string;  // eg TypeInfo( aSortPref )
function   EnumToSL( pEnumTypePtr : PTypeInfo ) : TStringList;
function   FindEnum( nam : string; pEnumTypePtr : PTypeInfo ) : int;
//function   Trim( const s : string ) : string;  use SysUtils
procedure  TrimThis( var s : string );

procedure  CopyStrList( const src : TStringList; dest : TStringList );
procedure  MakeUniqueSL( var src : TStringList );
function   ListFiles( const path, ext : string ) : TStringList;
function   FileOpenRead( name : string ) : THandle;
function   FileOpenWrite( name : string ) : THandle;
function   WinError( var er : int ) : string;  overload;
function   WinError() : string;  overload;

function   Minutes() : int;
function   DTToStr (dt: TDateTime): string;
function   ShortDateTime( dt : TDateTime ) : string;
function   TimeDateToStr( t : TDateTime; fmt : string = '' ) : string;  // 'hh:nn ddd d mmm yy'

function	ParseIPs( const ips : string ) : TList; // of int
function	IPtoStr( ipn : int ) : string;
function	StrToIP( ip : string ) : int;
function	IntToBin( val : card; digs : card ) : string;
procedure	SetEmptyPCharLength( var s : string; len : card );    // see ComputerName below
procedure	PCharToStr( var s : string; len : card );  // see ComputerName below
function	ComputerName : string;
function	GetIPFromHost( var HostName, IPaddr, WSAErr: string ): Boolean;

function   BackUpFileName( fn : string ) : string;   // .~ext
function   FileToStr( const fn : string ) : string;
function   SetPowerOf2( n : int ) : int;
function   NewZ( size : integer ) : pointer;    // get a zero filled mem block
procedure	WriteRegistry( base, name, val : string );
function	ReadRegistry( base, name : string ) : string;
function	KeyboardState() : asKeyState;


type
    aOnEachFile = procedure( name : string ) of object;

function	CharsToStr( const buf : array of char ) : string;
function	NewestFile( directory, match : string; numbered : boolean = false  ) : string;
function	SlashDirectory( directory : string ) : string;
function	OldestFile( directory, match : string; numbered : boolean = false ) : string;
function	CountFiles( directory, match : string; numbered : boolean = false ) : int;
procedure	ScanDirectory( directory, match : string; OnEachFile : aOnEachFile; subs : boolean = true );
function	AnyDecimal( str : string ) : int;
function	ExtractOnlyFileName( fn : string ) : string;
function	ReplaceExtension( name, ext : string ) : string;
procedure	EndSlashPath( dir : string );
function	PathList( path : string ) : TStringList;
function	PathFromList( path : TStringList ) : string;
function    GetExePath() : string;
procedure	WindowsOpenFile( name : string );  // shellexecute

type
{$POINTERMATH ON}
    apInt = ^ Int32;
{$POINTERMATH OFF}

function	RefCount( s : string ) : integer;      // WARNING low level string manipulation
procedure	BumpRefCount( s : string; offset : integer );


implementation


uses
	Windows, ShellAPI, Character, Winsock, ShlObj, Registry, ASCII, Dialogs; // Forms,


// _______________________________ S T R E A M __________________________________

constructor cTextFileStream.Create( name : string );

	begin
    mName := name;
    //AssignFile( mTF, name );
    // Reset( mTF );
    //mX := 1;

    end;


destructor	cTextFileStream.Destroy;

	begin
    if mDirection = sdWrite  then  begin
    	WriteLn( mTF, mBuf );
    	CloseFile( mTF );
    	end;
    oFile.Free;
    end;


function    cTextFileStream.Read( var ch : char ) : boolean;

	var
    	h  : THandle;
        buf : array [ 0..3 ] of byte;
        skip, c : integer;
	begin
    if mDirection <> sdRead  then  begin        // self initialize     InStream := TStringStream.Create(S, TEncoding.UTF8);
        mDirection := sdRead;
        h := FileOpenRead( mName );                       // check header for clues of file type
        skip := 0;
        try
        	try
                c := FileRead( h, buf[ 0 ], SizeOf( buf ) );

                mEncoding := enUTF8;
                if ( c >= 2 ) and ( buf[ 0 ] = $FF ) and ( buf[ 1 ] = $FE ) then  begin  // BOM = U+FEFF
                    skip := 2;
                    mEncoding := enUTF16;
                    end
                else if ( c >= 2 ) and ( buf[ 0 ] >= Ord( ' ' ) ) and ( buf[ 1 ] = 0 ) and ( buf[ 3 ] = 0 ) then  begin  // '<?xml.....    ( buf[ 0 ] = Ord( '<' ) ) and
                    mEncoding := enUTF16;
                    end
                else if ( c >= 3 ) and ( buf[ 0 ] = $EF ) and ( buf[ 1 ] = $BB ) and ( buf[ 2 ] = $BF ) then  begin  // BOM = U+FEFF
                    skip := 3;
                    mEncoding := enUTF8;
                    end;
            except  ShowMessage( 'could not open ' + mName );  // $$$

            end;
        finally
        	CloseHandle( h );
        	end;

        oFile := TFileStream.Create( mName, fmOpenRead );
        oFile.Read( buf, skip );  // skip BOM
        end;

    case mEncoding of
    	enUTF16 : begin
        	result := oFile.Read( ch, SizeOf( char ) ) = SizeOf( char );
        	end
        else  begin  //  enUTF8
            result := UTF8toChars( oFile, ch );
        	end;
    	end;
    end;



function    cTextFileStream.Write( ch : char ) : boolean;   // not tested

	begin
    if mDirection <> sdWrite  then  begin  AssignFile( mTF, mName );  Rewrite( mTF );  mDirection := sdWrite;  end;

    result := true;
    mBuf := mBuf + ch;
    if ch = lf then  begin
        WriteLn( mTF, mBuf );
        mBuf := '';
        end;
    end;


constructor cStringStream.Create( source : string );

	begin
    mBuf := source;
    mX := 1;
    end;


destructor	cStringStream.Destroy;

	begin
    end;


function    cStringStream.Read( var ch : char ) : boolean;

	begin
    if mX <= Length( mBuf ) then  begin
        result := true;
        ch := mBuf[ mX ];
        Inc( mX );
        end
    else   result := false;
    end;


function    cStringStream.Write( ch : char ) : boolean;   //override;   // not tested

	begin
    result := true;
    mBuf := mBuf + ch;
    end;

// _______________________________ A R I T H M E T I C __________________________


function   Min( a, b : int ) : int;

	begin
    if a > b then result := b
    else  result := a;
    end;


function   Max( a, b : int ) : int;

	begin
    if a > b then result := a
    else  result := b;
    end;


function    CountBits( val : card ) : card;    // Counting bits set, Brian Kernighan's way
                                               // http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetNaive
	begin
    result := 0;
    while val <> 0 do  begin
    	val := val and ( val - 1 );
        Inc( result );
    	end;
    end;


function	RectWH( ALeft, ATop, w, h : Integer ) : TRect;

    begin
    Result.Left := ALeft;
    Result.Top := ATop;
    Result.Right := ALeft + w - 1;
    Result.Bottom := ATop + h - 1;
    end;


function   NewZ( size : integer ) : pointer;    // get a zero filled mem block

	begin                        // use FreeMem to dispose of AllocMem
	Result := AllocMem( size );  // fills with zeros;  //GetMemory( size );
	//FillChar( Result^, size, 0 );
	end;


procedure	WriteRegistry( base, name, val : string );

	var
        r : TRegistry;
	begin     // eg RegistryBase = 'Software\\Alphasoft\\PhotoCat\\';  regWd := ReadRegistry( RegistryBase, 'WorkDirectory' );
    r := TRegistry.Create( KEY_ALL_ACCESS or KEY_WOW64_64KEY );   // for win7/64 system
    try
        r.RootKey := HKEY_CURRENT_USER;
        if r.OpenKey( base, true )  then  begin
            r.WriteString( name, val );
            end;
    finally
        r.Free;
    	end;
	end;


function	ReadRegistry( base, name : string ) : string;

	var
        r : TRegistry;
	begin
    result := '';
    r := TRegistry.Create( KEY_ALL_ACCESS or KEY_WOW64_64KEY );   // for win7/64 system
    try
        r.RootKey := HKEY_CURRENT_USER;
        if r.OpenKey( base, false )  then  begin
            result := r.ReadString( name );
            end;
    finally
    	r.Free;
    	end;
	end;


function	KeyboardState() : asKeyState;

    var
      ks : TKeyboardState;
  begin
  result := [];
  GetKeyboardState( ks );
  if ( ks[ vk_Shift ] and $80 ) <> 0 then  Include( result, ksShift );
  if ( ks[ vk_Menu ] and $80 ) <> 0 then  Include( result, ksAlt );
  if ( ks[ vk_Control ] and $80 ) <> 0 then  Include( result, ksControl );
  end;


procedure   LeaveOnly( leave : string; var s : string );

    var
    	i : int;
	begin
    for i := Length( s ) downto 1 do  begin
        if Pos( s[ i ], leave ) <= 0 then  begin   // bum char
            Delete( s, i, 1 );
        	end;
    	end;
    end;


procedure  StripStart( var s : string );

    begin
    while ( Length( s ) > 0 ) and not IsAlpha( s[ 1 ] ) do  Delete( s, 1, 1 );
    end;


procedure  StripEnd( var s : string );

    begin
    while ( Length( s ) > 0 ) and not IsAlpha( s[ Length( s ) ] ) do  Delete( s, Length( s ), 1 );
    end;


procedure  SkipSpace( const s : string; var index : integer );

	var
		x : int;
	begin
	x := index;
	while ( x <= Length( s ) ) and ( s[ x ] <= ' ' ) do  Inc( x );
	index := x;
	end;


function   WithoutSpaces( const s : string ) : string;

	var
		x : int;
		c : char;
	begin
	Result := '';
	for x := 1 to Length( s ) do  begin
		c := s[ x ];
		if c <= ' ' then  c := '_';
		Result := Result + c;
		end;
	end;


//function   Trim( const s : string ) : string;
//
//	var
//		x : int;
//	begin
//	result := s;
//	x := 1;
//	SkipSpace( result, x );
//	if x > 1 then  Delete( result, 1, x - 1 );
//	for x := Length( result ) downto 1 do  begin
//		if result[ x ] > ' ' then  begin
//			if x < Length( result ) then  begin
//				Delete( result, x + 1, Length( result ) - x );
//				end;
//			break;
//			end;
//		end;
//	end;


procedure  TrimThis( var s : string );

	var
		x : int;
	begin
	x := 1;
	SkipSpace( s, x );
	if x > 1 then  Delete( s, 1, x - 1 );
	for x := Length( s ) downto 1 do  begin
		if s[ x ] > ' ' then  begin
			if x < Length( s ) then  begin
                Delete( s, x + 1, Length( s ) - x );
	            end;
        	break;
            end
		end;
	end;


procedure  WindowsPath( var s : string );

	var
        i : int;
    begin
    while true do  begin
    	i := Pos('/', s);
    	if i <= 0 then  break;
    	s[i] := '\';
	    end;
    end;


function   GetString( const s : string; var index : integer ) : string;

	var                 //  "asfasdf"  or  asdadsf   style
		x : int;
        c : char;
	begin
	Result := '';    x := index;
	while ( x <= Length( s ) ) and ( ( s[ x ] = ListSep ) or ( s[ x ] = ' ' ) ) do  begin
		Inc( x );
		end;
    if StrX( s, x ) = StringDelim then  begin
    	repeat
			Result := Result + s[ x ];
			Inc( x );
            c := StrX( s, x );
            if c = StringDelim then  begin
                Result := Result + c;
                Inc( x );
                break
            	end;
        	until c = nul;
		end
	else  begin
		while ( x <= Length( s ) ) do  begin
			Result := Result + s[ x ];
			Inc( x );
			end;
		end;
	index := x;
	end;


function   GetListItem( const s : string; var index : integer ) : string;

	var                 //  "asfasdf"  or  asdadsf, asdfsadf, asdfadf   style
		x : int;
        c : char;
	begin
	Result := '';    x := index;
	while ( x <= Length( s ) ) and ( ( s[ x ] = ListSep ) or ( s[ x ] = ' ' ) ) do  begin
		Inc( x );
		end;
    if StrX( s, x ) = StringDelim then  begin
    	repeat
			Result := Result + s[ x ];
			Inc( x );
            c := StrX( s, x );
            if c = StringDelim then  begin
                Result := Result + c;
                Inc( x );
                break
            	end;
        	until c = nul;
		end
	else  begin
		while ( x <= Length( s ) ) and ( s[ x ] <> ListSep ) do  begin
			Result := Result + s[ x ];
			Inc( x );
			end;
		end;
	index := x;
	end;


function  Slice( const src : string; index : integer; count : integer ) : string;

	var
		i, j : integer;
	begin
    result := '';
    if index <= Length( src ) then  begin
        i := Length( src ) - index + 1;
        if i > count then  i := count;
        SetLength( result, i );
        for j := 1 to i do  result[ j ] := src[ index + j - 1 ];
    	end;
	end;


function   PosN( const substr, src : string; limit : integer ) : int;

	var
		i, x : int;
		f : char;
		found : boolean;
	begin
	result := 0;
	if ( substr <> '' ) and ( Length( src ) >= Length( substr ) ) then  begin
		f := substr[ 1 ];   result := 0;
		if Length( src ) < Length( substr ) + limit - 1 then  limit := Length( src ) - Length( substr ) + 1;

		for i := 1 to limit do  begin
			if src[ i ] = f then  begin  // first char match
				found := true;
				for x := 2 to Length( substr ) do  begin
					if src[ i + x - 1 ] <> substr[ x ] then  begin
						found := false;
						break;
						end;
					end;
				if found then  begin
					result := i;
					break;
					end;
				end;
			end;
		end;
	end;


function   IsAlpha( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'a' ) and ( c <= 'z' ) then   Result := true
	else if ( c >= 'A' ) and ( c <= 'Z' ) then   Result := true;
	end;


function   IsAlphaN( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'a' ) and ( c <= 'z' ) then   Result := true
	else if ( c >= 'A' ) and ( c <= 'Z' ) then   Result := true
    else if ( c >= '0' ) and ( c <= '9' ) then  result := true;
	end;


function   IsDecimal( c : char ) : boolean;

  begin
  Result := ( c >= '0' ) and ( c <= '9' );
  end;


function   IsLowerCase( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'a' ) and ( c <= 'z' ) then   Result := true
	end;


function   IsUpperCase( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'A' ) and ( c <= 'Z' ) then   Result := true
	end;


function	IsAlphaNumeric( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'A' ) and ( c <= 'Z' ) then   Result := true
	else if ( c >= 'a' ) and ( c <= 'z' ) then   Result := true
	else if ( c >= '0' ) and ( c <= '9' ) then   Result := true;
	end;


function	StrIndex( s : string; x : int = -1 ) : char;

	begin
    result := nul;
    if x = -1 then  x := Length( s );
    if ( Length( s ) > 0 ) and ( Length( s ) >= x ) then  result := s[ x ];  // Length( s ) ];
    end;


function   GetAlphas( const s : string; var index : integer ) : string;

	var
		x : int;
	begin
	Result := '';    x := index;
	if ( x <= Length( s ) ) and ( s[ x ] = ListSep ) then  begin
		Inc( x );
		end;
	while ( x <= Length( s ) ) and ( s[ x ] <> ListSep ) do  begin
		if IsAlpha( s[ x ] ) then	Result := Result + s[ x ]
		else  break;
		Inc( x );
		end;
	index := x;
	end;


function   Gap( const s : string ) : string;

	var             // typically put a space into flight name before numeric part
		x : int;
	begin
	result := s;
	if ( Length( s ) > 1 ) then  begin  //  and ( IsAlpha( s[ 1 ] ) )
		for x := 3 to Length( s ) do  begin
			if not IsAlpha( s[ x ] ) then  begin
				Insert( ' ', result, x );
				break;
				end;
			end;
		end;
	end;


function	GetHexNibble( ch : char; out val : int ) : boolean;

	begin
    result := true;
    if ( ch >= '0' ) and ( ch <= '9' ) then  val := Ord( ch ) - Ord( '0' )
    else if ( ch >= 'A' ) and ( ch <= 'F' ) then  val := Ord( ch ) - Ord( 'A' ) + $A
    else if ( ch >= 'a' ) and ( ch <= 'f' ) then  val := Ord( ch ) - Ord( 'a' ) + $A
    else  result := false;
    end;


function	GetHexByte( const s : string; var x : int ) : byte;

	var
    	val : int;
	begin
    result := 0;
    if GetHexNibble( StrX( s, x ), val ) then  begin
    	result := val;
    	Inc( x );
        if GetHexNibble( StrX( s, x ), val ) then  begin
            result := result * 16 + val;
    		Inc( x );
        	end;
    	end;
    end;


function   GetHex( const s : string; var index  : integer ) : int;  //  F00BAA or , $F00BAA

	var
		x, val : int;
	begin
	Result := 0;    x := index;
	while ( x <= Length( s ) ) and
			( ( s[ x ] <= ' ' ) or ( s[ x ] = ListSep ) or ( s[ x ] = '$' ) ) do  begin
		Inc( x );
		end;
	while ( x <= Length( s ) ) and ( s[ x ] <> ListSep ) do  begin
        if GetHexNibble( s[ x ], val ) then  Result := Result * 16 + val
//		if ( s[ x ] >= '0' ) and ( s[ x ] <= '9' )  then  begin
//			Result := Result * 16 + ord( s[ x ] ) - ord( '0' );
//			end
//		else  begin
//			c := ToUpper( s[ x ] );
//			if ( c >= 'A' ) and ( c <= 'F' ) then  begin
//				Result := Result * 16 + ord( c ) + 10 - ord( 'A' );
//				end
		else  break;
			//end;
		Inc( x );
		end;
	index := x;
	end;


function   GetInt( const s : string; var index : integer ) : integer;

	var
		x : int;
        negate : boolean;
	begin
	Result := 0;    x := index;    negate := false;
	while ( x <= Length( s ) ) and
			( ( s[ x ] <= ' ' ) or ( s[ x ] = ListSep ) ) do  begin
		Inc( x );
		end;
	if x <= Length( s ) then  begin
        if s[ x ] = '-' then  begin
            negate := true;
            Inc( x );
        	end;
		if s[ x ] = '$' then  begin
			result := GetHex( s, x );
			end
		else  begin
			while ( x <= Length( s ) ) and
					( s[ x ] >= '0' ) and ( s[ x ] <= '9' )  do  begin
				result := result * 10 + ord( s[ x ] ) - ord( '0' );
				Inc( x );
				end;
			end;
		end;
	index := x;
    if negate then result := - result;
	end;


function   GetBin( const s : string; var index  : integer ) : int;


	var
		x : int;
	begin
	Result := 0;    x := index;
	while ( x <= Length( s ) ) and
			( ( s[ x ] <= ' ' ) or ( s[ x ] = ListSep ) ) do  begin
		Inc( x );
		end;
	if x <= Length( s ) then  begin
        while ( x <= Length( s ) ) and
                ( s[ x ] >= '0' ) and ( s[ x ] <= '1' )  do  begin
            result := result * 2 + ord( s[ x ] ) - ord( '0' );
            Inc( x );
            end;
        end;
	index := x;
	end;


function  Decimal( const s : string; var index : cardinal ) : cardinal;

  begin
  Result := 0;
  while ( index <= cardinal( Length( s ) ) ) and
        ( ( s[ index ] = ' ' ) or ( s[ index ] = ',' ) ) do  begin
    Inc( index );
    end;
  while ( index <= cardinal( Length( s ) ) ) and
        ( s[ index ] >= '0' ) and ( s[ index ] <= '9' )  do  begin
    Result := Result * 10 + ord( s[ index ] ) - ord( '0' );
    Inc( index );
    end;
  end;


function  Decimal( const s : string;  var index : int ) : int;

  begin
  Result := 0;
  while ( index <= Length( s ) ) and
        ( ( s[ index ] = ' ' ) or ( s[ index ] = ',' ) ) do  begin
    Inc( index );
    end;
  while ( index <= Length( s ) ) and
        ( s[ index ] >= '0' ) and ( s[ index ] <= '9' )  do  begin
    Result := Result * 10 + ord( s[ index ] ) - ord( '0' );
    Inc( index );
    end;
  end;


function	StrX( const s : string; const x : int ) : char;

	begin
    if ( x >= 1 ) and ( x <= Length( s ) ) then  result := s[ x ]
    else  result := nul;
    end;


function	PosLast( ch : char;  const s : string ) : int;

	var
    	x : int;
	begin
    result := 0;
    for x := Length( s ) downto 1 do  begin
    	if s[ x ] = ch then  begin
        	result := x;
            break;
            end;
    	end;
    end;


function	Cash( amt : currency; show0 : boolean = false ) : string;

	begin
    if not show0 and ( amt = 0 ) then  result := ''
    else  result := Format( '%n', [ amt ] );    // was %f eg -1,234.56
    end;


function	TryCashToCurr( txt : string; var amt : currency ) : boolean;

    var
    	x : int;
        clean : string;

	begin
    result := true;   amt := 0;
    for x := 1 to Length( txt ) do  begin
        if IsDecimal( txt[ x ] ) then  clean := clean + txt[ x ]
        else if txt[ x ] = '.' then  clean := clean + '.'
        else if txt[ x ] = '-' then  clean := clean + '-'
        else if txt[ x ] = '$' then
        else if txt[ x ] = ',' then
        else  result := false;
    	end;
    if result then  result := TryStrToCurr( clean, amt );
    end;


function	IntToStrN( val, n : int ) : string;   // eg 007

	begin
    result := IntToStr( val );
    while Length( result ) < n do  Insert( '0', result, 1 );
    end;


function	UTF8toChars( fs : TStream; out ch : char ) : boolean;

	var
        b : byte;
        val : int;                  // http://en.wikipedia.org/wiki/UTF-8
	begin
    result := true;
    if fs.Read( b, 1 ) = 1 then  begin
    	if b and $80 = 0 then  begin   // top bit zero so just 7 bit ascii
        	ch := Char( b );
        	end
        else if b and $E0 = $C0 then  begin  // 2 byte form   11 bits
            val := ( b and $1F ) shl 6;
            if fs.Read( b, 1 ) = 1 then  begin
                ch := Char( val or b and $3F );
                end
            else  result := false;
        	end
        else if b and $F0 = $E0 then  begin  // 3 byte form   goes up to 16 bits
            val := ( b and $0F ) shl 12;
            if fs.Read( b, 1 ) = 1 then  begin
                val := val or (( b and $3F ) shl 6 );
                if fs.Read( b, 1 ) = 1 then  begin
                    ch := Char( val or b and $3F );
                    end
                else  result := false;
                end
            else  result := false;
        	end

//        else if b and $F8 = $F0 then  begin  // 4 byte form - makes 21 bits - not interested
//            val := ( b and $07 ) shl 18;     // limited to 21 bits ie 4 bytes to match UTF16 RFC3629
//            if fs.Read( b, 1 ) = 1 then  begin
//                val := val or (( b and $3F ) shl 12 );
//                if fs.Read( b, 1 ) = 1 then  begin
//                    val := val or (( b and $3F ) shl 6 );
//                    if fs.Read( b, 1 ) = 1 then  begin
//                        ch := Char( val or b and $3F );
//                        end
//                    else  result := false;
//                    end
//                else  result := false;
//                end
//            else  result := false;
//        	end
        else  result := false;  // illegal 4/5 byte form   ie EOF
    	end
    else  result := false;
    end;


function	ToDate( date : TDate ) : string;  overload;

	var
    	Y, M, D: Word;
    begin
	DecodeDate( date, Y, M, D );
    result := IntToStrN( Y, 4 ) + IntToStrN( M, 2 ) + IntToStrN( D, 2 );
    end;


function	ToDate( date : string ) : TDate;  overload;

	var
    	Y, M, D: Word;
        s : string;
        v : int;
	begin
    result := 0;
    if Length( date ) = 8 then  begin
        s := Copy( date, 1, 4 );
        if TryStrToInt( s, v ) then  begin
        	Y := v;
            s := Copy( date, 5, 2 );
            if TryStrToInt( s, v ) then  begin
            	M := v;
                s := Copy( date, 7, 8 );
                if TryStrToInt( s, v ) then  begin
                	D := v;
                    result := EncodeDate( Y, M, D );
                	end;
                end;
        	end;
    	end;
    end;


function   InList( const item, list : string ) : boolean;

	var              // is mCheckInName in supplied list of checkins
		p : int;
	begin
	result := false;
	p := Pos( item, list );
	if p > 0 then  begin   // worth testing
		if ( p = 1 ) or ( list[ p-1 ] = ListSep ) then  begin
			p := p + Length( item );
			if ( p > Length( list ) ) or ( list[ p ] = ListSep ) then  result := true;
			end;
		end;
	end;


function   ListIndex( const item, list : string ) : integer;

	var              // is mCheckInName in supplied list of checkins
		p : int;
        it : string;
        sl : TStringList;
	begin
	result := -1;
    it :=  LowerCase( item );
	p := 1;  sl := BuildParamsL( list, p );
    for p := 0 to sl.Count - 1 do  begin
        if it = LowerCase( sl[ p ] ) then  begin
            result := p;
            break;
        	end;
    	end;
	end;


function   ListItem( const list : string; index : int ) : string;

	var              // is mCheckInName in supplied list of checkins
		p : int;
        sl : TStringList;
	begin
	p := 1;  sl := BuildParamsL( list, p );
    result := ListItem( sl, index );
    sl.Free;
	end;


function   ListItem( const list : TStringList; index : int ) : string;

	begin
	result := '';
    if list <> nil then  begin
        if ( index >= 0 ) and ( index < list.Count ) then  result := list[ index ];
    	end;
	end;


function   BuildParamsL( const params : string; var index : int; sep : char = ListSep ) : TStringList;

	var
		pl : TStringList;
		nam, p : string;
		x : integer;
	begin
	pl := TStringList.Create;    x := index;
	if params <> '' then  begin
		p := params;
		// if p[ Length( p ) ] = ')' then  p[ Length( p ) ] := sep    else
		if p[ Length( p ) ] <> sep then   p := p + sep;

		SkipSpace( p, x );
		while x <= Length( p )  do  begin
			if p[ x ] = sep then  begin
                nam := Trim( nam );
				//if ( nam <> '' ) or ( sep = ListSep ) then  begin   // , lists allow empty item
					pl.Add( nam );
					nam := '';
				   //	end;
				Inc( x );
				SkipSpace( p, x );
				end
			else  begin
				nam := nam + p[ x ];
				Inc( x );
				end;
			end;
        //if nam <> '' then  pl.Add( nam );
		end;
	Result := pl;   index := x;
	end;


function   StrToList( const s : string ) : TStringList;

	var
		x : int;
	begin
	x := 1;
	result := BuildParamsL( s, x );
	end;


function   ListToStr( const list : TStringList ) : string;

	var
		x : int;
	begin
	result := '';
	//result := list.CommaText;  // quotes anything with a space
	for x := 0 to list.Count - 1 do  begin
        if x = 0 then	result := list[ x ]
        else            result := result + ',' + list[ x ]
		end;
	end;


function   ListToStr( const vals : array of int ) : string;   // overload;

	var
    	x : int;
	begin
    result := '';
    for x := 0 to High( vals ) do  begin
    	result := result + IntToStr( vals[ x ] );
        if x < High( vals ) then  result := result + ', ';
    	end;
    end;


function	ListsEqual( const listA, listB : TStrings ) : boolean;

    var
    	x : int;
	begin
    result := true;
    if listA.Count <> listB.Count then  result := false
    else  for x := 0 to listA.Count - 1 do  begin
        if listA[ x ] <> listB[ x ] then    begin
            result := false;
            break;
        	end;
    	end;
    end;


function    ExcludeFromList( const item, list : string ) : string;

    var
        x : int;
        l : TStringList;
	begin
    x := 1;
    l := BuildParamsL( list, x );
    x := 0;
    while x < l.Count do  begin
        if l[ x ] = item then  l.Delete( x )
        else Inc( x );
    	end;
    result := ListToStr( l );
    l.Free;
    end;


function    IncludeInList( const item, list : string ) : string;

    var
        x : int;
        l : TStringList;
        found : boolean;
	begin
    x := 1;
    l := BuildParamsL( list, x );
    x := 0;   found := false;
    for x := 0 to l.Count - 1 do  begin
        if l[ x ] = item then  begin
            found := true;
            break;
        	end;
    	end;
    if not found then  begin
         while true do  begin
            if x = l.Count then  begin
                l.Add( item );
                break;
                end;
            if l[ x ] = item then  break;
            if l[ x ] > item then  begin   // alpha sorted
                l.Insert( x, item );
                break;
                end;
            Inc( x );
            end;
    	end;
    result := ListToStr( l );
    l.Free;
    end;


function   GetLine( const s : string; var index : integer ) : string;

	var
		x : int;
	begin
	x := index;  result := '';
	while x <= Length( s ) do  begin
        if s[ x ] = #13 then  begin  Inc( x );  break;  end;
        if s[ x ] = #10 then    begin  end
		else  Result := Result + s[ x ];
		Inc( x );
		end;
	index := x;
	end;


function   LineCount( const s : string ) : int;

	var
		x : int;
        trailingCh : boolean;
	begin
	result := 0;  trailingCh := false;   x := 1;
	while x <= Length( s ) do  begin
        if s[ x ] = #13 then  begin  Inc( result ); trailingCh := false; end
        else if s[ x ] = #10 then  begin end
		else  trailingCh := true;      // catch any unterminated line
		Inc( x );
		end;
	if trailingCh then  Inc( result );
	end;


procedure  CopyStrList( const src : TStringList; dest : TStringList );

	var
		i : int;
	begin
	dest.Clear;
	for i := 0 to src.Count - 1 do  dest.Add( src[ i ] );
	end;


procedure  MakeUniqueSL( var src : TStringList );

	var
    	m, d : int;
	begin
    for m := src.Count - 1 downto 0 do  begin
        for d := src.Count - 1 downto m + 1 do  begin
            if src[ m ] = src[ d ]  then  src.Delete( d );
        	end;
    	end;
    end;


function    NoGapLower( s : string; lower : boolean = true ) : string;

    var
        x : int;
        c : char;
	begin
    result := '';
    for x := 1 to Length( s ) do  begin
        c := s[ x ];
        if c <> ' ' then  begin
            if lower then  result := result + LowerCase( c )  else  result := result + c;
        	end;
        end;
    end;


function  UnCamel( const n : string; gapChar : char = ' ' ) : string;

	var
		x : int;
		first : boolean;
	begin
	x := 1;    result := '';   first := true;
	while ( x <= Length( n ) ) and ( IsLowerCase( n[ x ] ) ) do  Inc( x );  // throw leading lowers
	while x <= Length( n ) do  begin
		if IsUpperCase( n[ x ] ) and not IsUpperCase( StrX( n, x - 1 ) ) then  begin
			if not first and ( gapChar <> #0 ) then  result := result + gapChar;
			first := false;
			end;
		result := result + n[ x ];
		Inc( x );
		end;
	if result = '' then  result := n;
	end;


function   EnumToStr( val : Integer; pEnumTypePtr : PTypeInfo; unCaml : boolean = true ) : string;  // stolen from Csi

	var           // eg str := EnumToStr( Ord( GetSortPref ), TypeInfo( aSortPref ) );
		lEnumDataPtr : PTypeData;
	begin
    result := '';
    if pEnumTypePtr.Kind = tkEnumeration then  begin
        lEnumDataPtr := GetTypeData( pEnumTypePtr );
        if ( val >= lEnumDataPtr.MinValue) and ( val <= lEnumDataPtr.MaxValue ) then  begin
            result := GetEnumName( pEnumTypePtr, val );
            if unCaml then  result := UnCamel( Result );
            end;
    	end;
	end;


function   EnumToSL( pEnumTypePtr : PTypeInfo ) : TStringList;

	var
		lEnumDataPtr : PTypeData;
        x : int;
	begin
    result := TStringList.Create;
	lEnumDataPtr := GetTypeData( pEnumTypePtr );
	for x := lEnumDataPtr.MinValue to lEnumDataPtr.MaxValue do  begin
        result.Add( UnCamel( GetEnumName( pEnumTypePtr, x ) ) );
    	end;
    end;


function   FindEnum( nam : string; pEnumTypePtr : PTypeInfo ) : int;

	var
		lEnumDataPtr : PTypeData;
        x : int;
	begin
    result := -1;
    if pEnumTypePtr.Kind = tkEnumeration then  begin
        lEnumDataPtr := GetTypeData( pEnumTypePtr );
        for x := lEnumDataPtr.MinValue to lEnumDataPtr.MaxValue do  begin
        	if nam = UnCamel( GetEnumName( pEnumTypePtr, x ) )  then  begin
            	result := x;
                break;
            	end;
            end;
	    end;
    end;

// _________________________ TIME ______________________________________________



function D (Val: byte): char;
	begin
		D := chr ((Val) + ord ('0'));
	end;


function DD (Val: byte): string;
	begin
		//if Val <= 99 then
			DD := D (Val div 10) + D (Val mod 10)
		//else
			//DD := '??';
	end;


function DDD (Val: word): string;
	begin
		DDD := DD (Val div 10) + D (Val mod 10);
	end;


function DDDD (Val: word): string;
	begin
		DDDD := DD (Val div 100) + DD (Val mod 100);
	end;


function DTToStr (dt: TDateTime): string;
	var
		yr, mo, dy: word;
		hr, mn, se, ms: word;
	begin
    Result := '';
    if dt <> 0.0 then  begin
        try
            DecodeDate (dt, yr, mo, dy);
            DecodeTime (dt, hr, mn, se, ms);
        except
        	end;
		Result := DDDD (yr) + DD (mo) + DD (dy);
        // if hr + mn + se <> 0 then  begin   // todo remove test, round out seconds
            Result := Result + ' ' + DD (hr) + DD (mn) + DD (se);
	        // end;
    	end;
	end;


function   Minutes() : int;

	var
		t : tDateTime;
		Hour, Min, Sec, MSec : word;
	begin
	t := Now;
	DecodeTime( t, Hour, Min, Sec, MSec );
	Result := Min;
	end;


function   ShortDateTime( dt : TDateTime ) : string;

	var
		fs : TFormatSettings;
	begin
    fs.ShortDateFormat := 'yyyymmdd';
    fs.LongTimeFormat := 'hhnnss';
    result := DateTimeToStr( dt, fs );
    end;


function   TimeDateToStr( t : TDateTime; fmt : string = '' ) : string;  // 'hh:nn ddd d mmm yy'

	var
		st : string;
	begin
	if fmt = '' then  fmt := 'hh:nn ddd d mmm yy';
    if t = 0.0 then   t := Now();
    DateTimeToString( st, fmt, t );
	result := st;
	end;


// _____________________________ IP ____________________________________________

function   ParseIPs( const ips : string ) : TList;
	var

		x : int;
		ip, ip2 : int;
		ipl : TList;

	procedure  SkipSpace();

		begin
		while ( ips[ x ] = ' ' ) and ( x <= Length( ips ) ) do  Inc( x );
		end;

	function   GetByte() : int;

		var
			b : string;
			i : int;
		begin
		while ( ips[ x ] >= '0' ) and ( ips[ x ] <= '9' ) and ( x <= Length( ips ) ) do  begin
			b := b + ips[ x ];
			Inc( x );
			end;
		i := 1;
		Result := GetInt( b, i );
		if ( Result < 0 ) or ( Result > 255 ) then  begin
			// Result := 0;
			raise Exception.Create('Invalid IP');
			end;
		end;

	function   GetIP() : int;

		var
			n : int;
			begin
		SkipSpace;
		Result := 0;
		for n := 0 to 3 do  begin
			Result := Result * 256 + GetByte();
			if n < 3 then  begin
				if ips[ x ] <> '.' then  raise Exception.Create('Invalid IP');
				Inc( x );
				end;
			 end;
		end;

	begin  // 192.168.0.20-23, 192.168.0.34 -> list of ints
	ipl := TList.Create;
	x := 1;
	while x <= Length( ips ) do  begin
		ip := GetIP();
		ipl.Add( Pointer( ip ) );
		SkipSpace;
		if x <= Length( ips ) then  begin
			if ips[ x ] = '-' then  begin
				Inc( x );
				ip2 := ip and Int( $FFFFFF00 ) + GetByte();
				if ip2 <= ip then  raise Exception.Create('Invalid IP range');
				repeat
					Inc( ip );
					ipl.Add( Pointer( ip ) );
					until ip = ip2;
				SkipSpace;
				end;
			if x > Length( ips ) then  break;
			if ips[ x ] <> ',' then  break;
			Inc( x );
			end;
		end;
	Result := ipl;
	end;


function   StrToIP( ip : string ) : int;

	var
    	x, b : int;
    begin
    x := 1;   result := 0;
    for b := 3 downto 0 do  begin
    	result := result + ( Decimal( ip, x ) shl ( 8 * b ) );
        Inc( x );
    	end;
    end;


function   IPtoStr( ipn : int ) : string;

	begin
	Result := IntToStr( ipn and $FF000000 shr 24 );
	Result := Result + '.' + IntToStr( ipn and $00FF0000 shr 16 );
	Result := Result + '.' + IntToStr( ipn and $0000FF00 shr 8 );
	Result := Result + '.' + IntToStr( ipn and $000000FF );
	end;


function	IntToBin( val : card; digs : card ) : string;

    var
        mask, d : card;
	begin
    result := '';  mask := 1 shl ( digs - 1 );
    for d := 1 to digs do  begin
        if val and mask <> 0 then  result := result + '1'  else  result := result + '0';
        mask := mask shr 1;
    	end;
    end;


procedure	SetEmptyPCharLength( var s : string; len : card );    // use a string as an empty pchar buffer, see ComputerName below

	begin
    SetLength( s, len );
    s[ 1 ] := #0;
    end;


procedure	PCharToStr( var s : string; len : card );  // convert PChar buffer back to string, see ComputerName below

    var
        i : card;
	begin
    for i := 1 to Cardinal( Length( s ) ) do  begin
        if s[ i ] = #0 then  begin   // strlen()
            SetLength( s, i - 1 );
            break;
            end;
        end;
	end;


function	ComputerName : string;

    var
        i : card;
	begin
    i := 256;
    SetEmptyPCharLength( result, i );
    if GetComputerName( @result[ 1 ], i ) then  begin
    	PCharToStr( result, i );
    	end;
    end;



// http://delphi.about.com/od/networking/l/aa103100a.htm

function GetIPFromHost( var HostName, IPaddr, WSAErr: string ): Boolean;
type
  Name = array[0..100] of Char;
  PName = ^Name;
var
  HEnt: pHostEnt;
  AnsiHostName : Ansistring;
  WSAData: TWSAData;
  i: Integer;
begin
  Result := False;
  if WSAStartup($0101, WSAData) <> 0 then begin
    WSAErr := 'Winsock is not responding."';
    Exit;
  end;
  IPaddr := '';
  SetLength( AnsiHostName, 256 );  AnsiHostName[ 1 ] := #0;
  if GetHostName(@AnsiHostName[ 1 ], 256 ) = 0 then    //  GetHostName(HName^, SizeOf(Name)) = 0 then
  	begin
    for i := 1 to Cardinal( Length( AnsiHostName ) ) do  begin
        if AnsiHostName[ i ] = #0 then  begin   // strlen()
            SetLength( AnsiHostName, i - 1 );
            break;
            end;
        end;


    HostName := String( AnsiHostName );
    HEnt := GetHostByName(@AnsiHostName[ 1 ]);
    for i := 0 to HEnt^.h_length - 1 do
     IPaddr :=
      Concat(IPaddr,
      IntToStr(Ord(HEnt^.h_addr_list^[i])) + '.');
    SetLength(IPaddr, Length(IPaddr) - 1);
    Result := True;
  end
  else begin
   case WSAGetLastError of
    WSANOTINITIALISED:WSAErr:='WSANotInitialised';
    WSAENETDOWN      :WSAErr:='WSAENetDown';
    WSAEINPROGRESS   :WSAErr:='WSAEInProgress';
   end;
  end;
  WSACleanup;
end;


//________________________________ FILES _______________________________________



function  BackUpFileName( fn : string ) : string;   // .~ext

	var
		ext : string;
	begin
	result := fn;
	ext := ExtractFileExt( fn );
	if ( Length( ext ) >= 2 ) and ( ext[ 1 ] = '.' ) then  begin
		Insert( '~', ext, 2 );
		result := ChangeFileExt( fn, ext );
		end
	else  result := fn + '.bak';
	end;


function   ListFiles( const path, ext : string ) : TStringList;

	var
		sr: TSearchRec;
		FileAttrs: Integer;
		p : string;
	begin
	p := path;
	result := TStringList.Create;    result.Sorted := true;
	if ( path <> '' ) and ( path[ 1 ] = '\' ) then  p := GetCurrentDir + path;
	if ext <> '' then  begin
		if ext[ 1 ] = '.' then  p := p + '*' + ext  else  p := p + '*.' + ext;
		end
	else   p := p + '*.*';

	FileAttrs := faAnyFile;
	if SysUtils.FindFirst( p, FileAttrs, sr) = 0 then  begin
		repeat
			if sr.Attr and faDirectory = 0 then  result.Add( sr.Name );
			until SysUtils.FindNext(sr) <> 0;
		SysUtils.FindClose(sr);
		end;
	end;


function   FileToStr( const fn : string ) : string;

    var
        f : textfile;
        l : string;
	begin
    result := '';
    Assign( f, fn );
    Reset( f );
    while not EOF( f ) do  begin
        Readln( f, l );
        result := result + l + EOL;
    	end;
    CloseFile( f );
    end;


function  FileOpenRead( name : string ) : THandle;

	const
		GENERIC_WRITE =   $40000000;  // WinNT.h  CreateFile params
		GENERIC_READ  =   $80000000;
		FILE_SHARE_READ	= $00000001;  // WinNT.h
		OPEN_EXISTING   = 3;          // WinBase.h
		CREATE_ALWAYS   = 0;

	begin
	Result := CreateFile( PChar( name ), GENERIC_READ,
						FILE_SHARE_READ, Nil, OPEN_EXISTING, 0, 0);
	end;                  // FileSeek( h, skip, 0 );   c := FileRead( h, mStr[ 1 ], size );  CloseHandle( h );


function  FileOpenWrite( name : string ) : THandle;

	const
		GENERIC_WRITE =   $40000000;  // WinNT.h  CreateFile params
		GENERIC_READ  =   $80000000;
		FILE_SHARE_READ	= $00000001;  // WinNT.h
		OPEN_EXISTING   = 3;          // WinBase.h
		CREATE_ALWAYS   = 0;

	begin
	Result := CreateFile( PChar( name ), GENERIC_WRITE,
						FILE_SHARE_READ, Nil, CREATE_ALWAYS, 0, 0);
	end;                  // use CloseHandle


function	FileNameHasDecimal( fn : string ) : boolean;

	var
    	name : string;
        i : int;
	begin
    result := false;
	name := ExtractFileName( fn );
    for i := 1 to Length( name ) do  begin
        if IsDecimal( name[ i ] ) then  begin
            result := true;
            break;
        	end;
    	end;
    end;
{$warnings off }
{$ifdef FMX }

function	NewestFile( directory, match : string; numbered : boolean = false ) : string;

    var
        fil : TSearchRec;    // TimeStamp: TDateTime
        newest : TDateTime;
    begin
    if FindFirst( SlashDirectory( directory ) + match, $80, fil ) = 0  then  begin // faNormal
    	newest := 0;
        repeat
            if fil.TimeStamp > newest then  begin
            	if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                    newest := fil.TimeStamp;
                    result := fil.Name;
                	end;
            	end;
            until ( FindNext( fil ) <> 0 )
        FindClose( fil );
    	end;
    end;



function	OldestFile( directory, match : string; numbered : boolean = false ) : string;

    var
        fil : TSearchRec;
        oldest : TDateTime;
    begin
    if FindFirst( SlashDirectory( directory ) + match, $80, fil ) = 0  then  begin // faNormal
    	oldest := 1E99;    // maxint
        repeat
            if fil.TimeStamp < oldest then  begin
            	if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                    oldest := fil.TimeStamp;
                    result := fil.Name;
                	end;
            	end;
            until ( FindNext( fil ) <> 0 );
        FindClose( fil );
    	end;
    end;

{$else }

function	NewestFile( directory, match : string; numbered : boolean = false ) : string;

    var
        fil : TSearchRec;    // TimeStamp: TDateTime
        newest : int;
    begin
    if SysUtils.FindFirst( SlashDirectory( directory ) + match, $80, fil ) = 0  then  begin // faNormal
    	newest := 0;
        repeat
            if fil.Time > newest then  begin
            	if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                    newest := fil.Time;
                    result := fil.Name;
                	end;
            	end;
            until ( SysUtils.FindNext( fil ) <> 0 );
        SysUtils.FindClose( fil );
    	end;
    end;


function	OldestFile( directory, match : string; numbered : boolean = false ) : string;

    var
        fil : TSearchRec;
        oldest : int;
    begin
    if SysUtils.FindFirst( SlashDirectory( directory ) + match, $80, fil ) = 0  then  begin // faNormal
    	oldest := maxint;    // maxint
        repeat
            if fil.Time < oldest then  begin
            	if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                    oldest := fil.Time;
                    result := fil.Name;
                	end;
            	end;
            until ( SysUtils.FindNext( fil ) <> 0 );
        SysUtils.FindClose( fil );
    	end;
    end;


{$endif }


function	SlashDirectory( directory : string ) : string;

    begin
    result := directory;
    if result <> '' then  begin
        if result[ Length( result ) ] <> '\' then  result := result + '\' ;
        end;
    end;


function	CountFiles( directory, match : string; numbered : boolean = false ) : int;

    var
        fil : TSearchRec;
    begin
    result := 0;
    if SysUtils.FindFirst( SlashDirectory( directory ) + match, faNormal, fil ) = 0  then  begin // faNormal = $80
        repeat
            if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                Inc( result );
                end;
            until ( SysUtils.FindNext( fil ) <> 0 );
        SysUtils.FindClose( fil );
        end;
    end;


procedure	ScanDirectory( directory, match : string; OnEachFile : aOnEachFile; subs : boolean = true );

    var
        fil : TSearchRec;
        dir : string;
    begin
    dir := SlashDirectory( directory );
    if SysUtils.FindFirst( dir + match, faNormal, fil ) = 0  then  begin // $80 faNormal
        repeat
            OnEachFile( dir + fil.Name );  //  if fil.Attr = faNormal then
            until ( SysUtils.FindNext( fil ) <> 0 );
        SysUtils.FindClose( fil );
    	end;
    if subs then  begin
    	if SysUtils.FindFirst( dir + '*.*', faDirectory, fil ) = 0  then  begin
            repeat
            	if ( fil.Name <> '.' ) and ( fil.Name <> '..' ) then  begin
                	ScanDirectory( dir + fil.Name, match, OnEachFile, subs );  //  if fil.Attr = faNormal then
                    end;
                until ( SysUtils.FindNext( fil ) <> 0 );
        	end;
    	end;
    end;
{$warnings on }


function	AnyDecimal( str : string ) : int;

    var
        i, x : int;
    begin
    result := 0;
    for i := 1 to Length( str ) do  begin
        if IsDecimal( str[ i ] ) then  begin
        	x := i;
            result := GetInt( str, x );
            break;
        	end;
    	end;
    end;


function	ExtractOnlyFileName( fn : string ) : string;

	var              // strip directory, extension and decimal tail
    	p : int;
	begin
    result := ExtractFileName( fn );
    p := Pos( '.', fn );                        // strip extension
    if p > 0 then  Delete( result, p, 99 );
    for p := 1 to Length( result ) do  begin       // strip decimal
        if IsDecimal( result[ p ] ) then  begin
        	Delete( result, p, 99 );
            break;
        	end;
    	end;
    end;


function	ReplaceExtension( name, ext : string ) : string;

	var
    	old : string;
	begin
    old := ExtractFileExt( name );
    result := Slice( name, 1, Length( name ) - Length( old ) ) + ext;
    end;


function	CharsToStr( const buf : array of char ) : string;

    var
    	i : int;
	begin
    i := 0;    result := '';
    while ( i < High( buf ) ) and ( buf[ i ] <> nul )  do  begin
        result := result + buf[ i ];
        Inc( i );
    	end;
    end;


procedure	EndSlashPath( dir : string );

	begin
    if dir[ Length( dir ) ] <> '\' then  dir := dir + '\';
    end;


function	PathList( path : string ) : TStringList;

	var
    	x : int;
    begin
    x := 1;
    result := BuildParamsL( path, x, '\' );
    end;


function	PathFromList( path : TStringList ) : string;

    var
    	x : int;
    begin
    result := '';
    for x := 0 to path.Count - 1 do  begin
        result := result + path[ x ];
        if x <> path.Count - 1 then  result := result + '\';
    	end;
    end;


function    GetExePath() : string;

	begin
    result := ExtractFilePath( ParamStr(0) );
    end;


procedure	WindowsOpenFile( name : string );

	begin
    ShellExecute(0, 'OPEN', PChar( name ), '', '', SW_SHOWNORMAL);
    end;


function   WinError( var er : int ) : string;

	begin
	result := '';
	er := GetLastError();
	if er <> 0 then  begin
		result := SysErrorMessage( er ) + ' ER' + IntToStr( er );
		end;
	end;


function   WinError() : string;

	var
		error : int;
	begin
	result := WinError( error );
	end;


function   SetPowerOf2( n : int ) : int;

  	begin
    result := 1;
    while result < n do  begin
        result := result shl 1;
    	end;
  	end;



function	RefCount( s : string ) : integer;

	var
    	p : apInt;
	begin
    p := pointer( s );
    if p <> nil then    begin
        result := p[ -2 ];
    	end
    else  result := 0;
    end;


procedure	BumpRefCount( s : string; offset : integer );

	var
    	p : apInt;
	begin
    p := pointer( s );
    if p <> nil then  begin
        p[ -2 ] := p[ -2 ] + offset;
    	end;
    end;



{
BOOL ReadFile(
  HANDLE hFile,
  LPVOID lpBuffer,
  DWORD nNumberOfBytesToRead,
  LPDWORD lpNumberOfBytesRead,
  LPOVERLAPPED lpOverlapped
);
}
end.
